-module(gs_parser).

-include("termanus.hrl").

-export([gs_parse/0]).

gs_parse() ->
    lists:map(fun parse_file/1, erlout:get(analysed_files)).

parse_file(File) ->
    {ok, Src} = epp:parse_file(File, []),
	[Module] = [Mod#attribute.value || Mod = ?attr_module <- Src],
    Funs = [Fun || Fun <- Src, is_record(Fun, function)],
    [parse_fun(Fun, Module) || Fun <- Funs],
	%% module parsed. Syntax analysis.
	gs_starts(),
	gs_calls(cast),
	gs_calls(call).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Extraction of needed data from tree											%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_fun(Fun = #function{}, Module) ->
	[parse(Clause, {Module, Fun#function{enrtyes = [Clause#clause{tail = []}]}}) || Clause <- Fun#function.enrtyes].

parse(Element, Caller) when is_list(Element) ->
	[parse(A, Caller) || A <- Element];
	
parse(Element = #call{
                    who = #remote{
                        mod = #atom{val = gen_server},
                        func = #atom{}
                    }
                }, Caller) ->
	erlout:put(gs_links, [{Caller, Element}]),
    parse(Element#call.value, Caller);

parse(Element = #call{who = #atom{val = Fun}}, Caller = {Mod, #function{name = GSFun}})
            when GSFun == handle_call; GSFun == handle_cast ->
    erlout:put(gs_links, [{Caller, {Mod, Fun}}]),
	parse(Element#call.value, Caller);

parse(#remote{mod = #atom{val = Far}, func = #atom{val = FarFun}}, Caller = {_, #function{name = Fun}})
            when Fun == handle_call; Fun == handle_cast ->
	erlout:put(gs_links, [{Caller, {Far, FarFun}}]);

parse(Element, Caller) when is_tuple(Element) ->
	[parse(Chpok, Caller) || Chpok <- tuple_to_list(Element)];

parse(_, _) -> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server start function analysis											%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gs_starts() ->
	{Starts, NoStarts} = starts(erlout:get(gs_links)),
	{RegStarts, JustStarts} = lists:partition(fun reg_starts/1, Starts),
	lists:map(fun simple_logger:war_1_not_reg_start_gen_server/1, JustStarts),
	{Atomic, NonAtomic} = lists:partition(fun module_type/1, RegStarts),
	lists:map(fun simple_logger:war_2_gen_server_mod_arg_is_not_atom/1, NonAtomic),
	{Normal, Abst} = lists:partition(fun stabile_name/1, Atomic),
	lists:map(fun simple_logger:war_3_gen_server_name_is_not_stable/1, Abst),
	erlout:set(gs_ready, starts_linked(Normal)),
	erlout:set(gs_servers, lists:map(fun assoc_gs_names/1, Normal)),
	erlout:set(gs_links, NoStarts).

select_functions(List, Fun) ->
	lists:partition(fun(Link) ->
			{_, #call{who = #remote{func = #atom{val = Fun}}}} -> true;
			_ -> false;
		end,
		List).

starts(Link) ->
	{P1, T1} = select_functions(Link, start),
	{P2, T2} = select_functions(T1, start_link),
	{P3, T3} = select_functions(T2, start_monitor),
	{P1 ++ P2 ++ P3, T3}.

reg_starts({_, #call{value = Args}}) when length(Args) == 4 -> true;
reg_starts(_) -> false.

module_type({_, #call{value = Args}}) ->
	case lists:nth(2, Args) of
		#atom{} -> true;
		_ -> false
	end.

stabile_name({_, #call{value = Args}}) ->
	[Name | _] = Args,
	try erl_parse:normalise(Name),
		true
	catch
		 _ -> false
	end.

assoc_gs_names({_, #call{value = Args}}) ->
	[Name, #atom{val = Module} | _] = Args,
	{Module, erl_parse:normalise(Name)}.

starts_linked(Links) ->
	Funs = lists:filter(
		fun({{_, _}, {_, _}}) -> false; (_) -> true end,
		erlout:get(links)),
	
	lists:filtermap(
		fun(Arg = {{Caller, #function{name = FunName}}, #call{value = Args}}) ->
			[_, #atom{val = Called} | _] = Args,
			case lists:member({Called, init}, Funs) of
				true ->
					%% start is atom now but it should be changed
					%% when separation on instances will be implemented
					{true, {{Caller, FunName}, {Called, init}, start}};
				false ->
					simple_logger:war_4__gen_server_init_not_found(Arg),
					false
			end
		end,

		Links).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server calls/casts analysis												%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gs_calls(Fun) ->
	{Calls, NoCalls} = select_functions(erlout:get(gs_links), Fun),
	{Normal, Abs} = lists:partition(fun stabile_call/1, Calls),
	lists:map(fun simple_logger:war_5_gen_server_cast_uncnown_server/1, Abs),
	lists:foldl(fun put_call/2, Fun, Normal),
	erlout:set(gs_links, NoCalls).

stabile_call({_, #call{value = Args}}) ->
	[Name | _] = Args,
	try erl_parse:normalise(Name),
		true
	catch
		 _ -> false
	end.

put_call({{Mod, #function{name = FunName}}, #call{value = [Name | _]}}, Type) ->
	erlout:put(gs_ready, [{{Mod, FunName}, {erl_parse:normalise(Name)}, Type}]), Type.
