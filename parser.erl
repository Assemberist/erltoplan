-module(parser).

-include("termanus.hrl").

-export([links/1, get_links/2, get_functions/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Interfaces																	%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_functions(File) ->
	{ok, Src} = epp:parse_file(File, []),
	[Fun#function.name || Fun <- Src, is_record(Fun, function)].

links(File) ->
	{ok, Src} = epp:parse_file(File, []),
	[Module] = [Mod#attribute.value || Mod = ?attr_module <- Src],
	Behavior = [gen_server || #attribute{type = behavior, value = gen_server} <- Src],
	
	FunList = [Fun || Fun <- Src, is_record(Fun, function)],
	[get_links(Fun, Module) || Fun <- FunList],
	TrueCalls = filter_std_funs(erlout:get(trash), [Fun#function.name || Fun <- FunList], Module),
	erlout:put(links, [{Module, Fun#function.name} || Fun <- FunList]),
	erlout:put(links, TrueCalls),
	erlout:put(analysed_files, [{Module, File, Behavior}]),
	erlout:set(trash, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% New idea to search on tree													%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_links(Fun = #function{}, Module) ->
	[get_links(Clause, {Module, Fun#function.name}) || Clause <- Fun#function.enrtyes];

get_links(Element, Caller) when is_list(Element) ->
	[get_links(A, Caller) || A <- Element];

get_links(Element = #call{who = #atom{val = Fun}}, Caller = {Mod, _}) ->
	erlout:put(trash, [{Caller, {Mod, Fun}}]),
	get_links(Element#call.value, Caller);
	
get_links(#remote{mod = #atom{val = Far}, func = #atom{val = FarFun}}, Caller) ->
	erlout:put(links, [#link{caller = Caller, called = {Far, FarFun}}]);

get_links(Element, Caller) when is_tuple(Element) ->
	[get_links(Chpok, Caller) || Chpok <- tuple_to_list(Element)];

get_links(_, _) -> [].

filter_std_funs(Calls, FunNames, Module) ->
	Funs = lists:map(
		fun({Caller, {M, F}}) ->
			Called = case M of
				Module -> 
					case lists:member(F, FunNames) of
						true -> {M, F};
						false -> {erlang, F}
					end;
				_ -> 
					{M, F}
			end,
			{Caller, Called}
		end,
		Calls
	),
	[#link{caller = Caller, called = Called} || {Caller, Called} <- Funs].