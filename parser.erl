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
	FunList = [Fun || Fun <- Src, is_record(Fun, function)],
	[get_links(Fun, Module) || Fun <- FunList],
	TrueCalls = filter_std_funs(erlout:get(trash), [Fun#function.name || Fun <- FunList], Module),
	erlout:put(links, TrueCalls),
	erlout:set(trash, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% New idea to search on tree													%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_links(Fun = #function{}, Module) ->
	[get_links(Clause, {Module, Fun#function.name}) || Clause <- Fun#function.enrtyes];

get_links(Element, Caller) when is_list(Element) ->
	[get_links(A, Caller) || A <- Element];

get_links(Element = #call{}, Caller = {Mod, _}) ->
	case Element#call.who of
		#atom{} -> 
			erlout:put(trash, [{Caller, {Mod, Element#call.who#atom.val}}]);
		_ -> 
			get_links(Element#call.who, Caller)
	end,
	get_links(Element#call.value, Caller);
	
get_links(Element = #remote{mod = Far, func = FarFun}, Caller) ->
	case {Far, FarFun} of
		{#atom{}, #atom{}} -> 
			erlout:put(links, [{Caller, {Far#atom.val, FarFun#atom.val}}]);
		_ -> 
			get_links(Element#remote.mod, Caller),
			get_links(Element#remote.func, Caller)
	end;

get_links(Element, Caller) when is_tuple(Element) ->
	[get_links(Chpok, Caller) || Chpok <- tuple_to_list(Element)];

get_links(_, _) -> [].

filter_std_funs(Calls, FunNames, Module) ->
	lists:map(
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
	).
