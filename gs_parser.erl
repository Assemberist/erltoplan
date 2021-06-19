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
	gs_analyse().

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
%% Analyse																		%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gs_analyse() ->
	{Starts, _} = lists:partition(fun starts/1, erlout:get(gs_links)),
	{RegStarts, JustStarts} = lists:partition(fun reg_starts/1, Starts),
	lists:map(fun simple_logger:war_1_not_reg_start_gen_server/1, JustStarts),
	{Atomic, NonAtomic} = lists:partition(fun module_type/1, RegStarts),
	lists:map(fun simple_logger:war_2_gen_server_mod_arg_is_not_atom/1, NonAtomic),
	erlout:set(gs_servers, lists:map(fun assoc_gs_names/1, Atomic)).

starts(Link) ->
	case Link of 
		{_, #call{
				who = #remote{
					mod = #atom{val = gen_server},
					func = #atom{val = start}
				}
		}} -> true;

		{_, #call{
				who = #remote{
					mod = #atom{val = gen_server},
					func = #atom{val = start_link}
				}
		}} -> true;
					
		{_, #call{
				who = #remote{
					mod = #atom{val = gen_server},
					func = #atom{val = start_monitor}
				}
		}} -> true;
					
		_ -> false
	end.

reg_starts({_, #call{value = Args}}) when length(Args) == 4 -> true;
reg_starts(_) -> false.

module_type({_, #call{value = Args}}) ->
	case lists:nth(2, Args) of
		#atom{} -> true;
		_ -> false
	end.

assoc_gs_names(Call) ->
	