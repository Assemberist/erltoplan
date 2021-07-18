-module(gs_parser).

-include("termanus.hrl").

-export([gs_parse/1]).

gs_parse(Files) ->
    lists:map(fun parse_file/1, Files),
	gs_starts(),
	gs_calls(cast, erlout:get(gs_casts)),
	gs_calls(call, erlout:get(gs_calls)),
	erlout:put(gs_ready, associate_gs_calls()),
	erlout:set(gs_casts, []),
	erlout:set(gs_calls, []),
	erlout:set(trash, []).	

parse_file(File) ->
    {ok, Src} = epp:parse_file(File, []),
	[Module] = [Mod#attribute.value || Mod = ?attr_module <- Src],
	Behavior = [Module || #attribute{type = behavior, value = gen_server} <- Src],
	erlout:put(analysed_files, Behavior),

    Funs = [Fun || Fun <- Src, is_record(Fun, function)],
    [parse_fun(Fun, Module) || Fun <- Funs].

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
                        func =  #atom{val = Val}
                    }
                }, Caller) ->
	case Val of
		start -> erlout:put(gs_starts, [{Caller, Element}]);
		start_link -> erlout:put(gs_starts, [{Caller, Element}]);
		start_monitor -> erlout:put(gs_starts, [{Caller, Element}]);
		call -> erlout:put(gs_calls, [{Caller, Element}]);
		cast -> erlout:put(gs_casts, [{Caller, Element}])
	end,
    parse(Element#call.value, Caller);

parse(Element, Caller) when is_tuple(Element) ->
	[parse(Chpok, Caller) || Chpok <- tuple_to_list(Element)];

parse(_, _) -> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server start function analysis											%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gs_starts() ->
	{RegStarts, JustStarts} = lists:partition(fun reg_starts/1, erlout:get(gs_starts)),
	lists:map(fun simple_logger:war_1_not_reg_start_gen_server/1, JustStarts),
	{Atomic, NonAtomic} = lists:partition(fun module_type/1, RegStarts),
	lists:map(fun simple_logger:war_2_gen_server_mod_arg_is_not_atom/1, NonAtomic),
	{Normal, Abst} = lists:partition(fun stabile_name/1, Atomic),
	lists:map(fun simple_logger:war_3_gen_server_name_is_not_stable/1, Abst),
	erlout:put(gs_ready, starts_linked(Normal)),
	erlout:put(gs_servers, lists:map(fun assoc_gs_names/1, Normal)),
	erlout:set(gs_starts, []).

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
	Funs = erlout:get(analysed_files),
	
	lists:filtermap(
		fun(Arg = {{Caller, #function{name = FunName}}, #call{value = [_, #atom{val = Called} | _]}}) ->
			case lists:member(Called, Funs) of
				true ->
					%% start is atom now but it should be changed
					%% when separation on instances will be implemented
					{true, {start, {Caller, FunName}, {Called, init}}};
				false ->
					simple_logger:war_4__gen_server_init_not_found(Arg),
					false
			end
		end,
		Links).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server calls/casts analysis												%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gs_calls(Fun, List) ->
	lists:foldl(fun check_call/2, Fun, List).

check_call(Call = {{Mod, #function{name = FunName}}, #call{value = [Name | _]}}, Type) ->
	try erlout:put(trash, [{{Mod, FunName}, erl_parse:normalise(Name), Type}])
	catch _ -> simple_logger:war_5_gen_server_cast_uncnown_server(Call)
	end, Type.

associate_gs_calls() ->	
	lists:filtermap(fun({Caller, Term, Type}) ->
		[TrueMod, _] = lists:foldl(fun({Mod, Name}, [CalledName, Template]) ->
			case Name of
				Template -> [Mod, Template];
				_ -> [CalledName, Template]
			end
		end,
		[[], Term],
		erlout:get(gs_servers)),
		
		case TrueMod of
			[] -> 
				simple_logger:war_5_gen_server_call_uncnown_server({Caller, Term, Type}),
				false;
			_ ->
				{true, {Type, Caller, {TrueMod, case Type of call -> handle_call; _ -> handle_cast end}}}
		end
	end,
	erlout:get(trash)).