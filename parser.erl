-module(parser).

-include("termanus.hrl").

-export([links/1, get_links/3, get_functions/1]).

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
	Calls = lists:merge([get_links(Fun#function.enrtyes, {Module, Fun#function.name}, Module) || Fun <- FunList]),
	TrueCalls = filter_std_funs(Calls, [Fun#function.name || Fun <- FunList], Module),
	erlout:set(links, TrueCalls).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% New idea to search on tree													%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_links(Element, FunName, Mod) when is_list(Element) ->
	lists:merge([get_links(A, FunName, Mod) || A <- Element]);

get_links(Element, FunName, Mod) when is_tuple(Element) ->
	NextName = case element(1, Element) of
		call ->
			case Element#call.who of
				#atom{} -> 
					{Mod, Element#call.who#atom.val};
				_ -> FunName
			end;
		remote ->
			case {Element#remote.mod, Element#remote.func} of
				{#atom{}, #atom{}} -> 
					{Element#remote.mod#atom.val, Element#remote.func#atom.val};
				_ -> FunName
			end;
		_ ->
			FunName
	end,

	NextElements = lists:merge([get_links(Chpok, NextName, Mod) || Chpok <- tuple_to_list(Element)]),
	case NextName of
		FunName -> NextElements;
		_ -> [{FunName, NextName} | NextElements]
	end;

get_links(_, _, _) -> [].

filter_std_funs(Calls, FunNames, Module) ->
	lists:map(
		fun({{M1, F1},{M2, F2}}) ->
			Caller = case M1 of
				Module -> 
					case lists:member(F1, FunNames) of
						true -> {M1, F1};
						false -> {erlang, F1}
					end;
				_ -> 
					{M1, F1}
			end,
			Called = case M2 of
				Module -> 
					case lists:member(F2, FunNames) of
						true -> {M2, F2};
						false -> {erlang, F2}
					end;
				_ -> 
					{M2, F2}
			end,
			{Caller, Called}
		end,
		Calls
	).
