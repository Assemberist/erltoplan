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
	%% Select functions that starts gen_server
	{Starts, Tail1} = lists:partition(fun(X) ->
			case X of 
				{_, #call{
							who = #remote{
								mod = #atom{val = gen_server},
								func = #atom{val = start}
							},
							value = Args
						 }} when length(Args) == 4 -> true;

				{_, #call{
							who = #remote{
								mod = #atom{val = gen_server},
								func = #atom{val = start_link}
								},
							value = Args
						 }} when length(Args) == 4 -> true;
						 
				{_, #call{
							who = #remote{
								mod = #atom{val = gen_server},
								func = #atom{val = start_monitor}
							},
							value = Args
						 }} when length(Args) == 4 -> true;
						 
				_ -> false
		end,
		erlout:get(gs_links)).