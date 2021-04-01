-module(parser).

-include("/home/sanya/source/erltoplan/termanus.hrl").

-export([parse/1, get_functions/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Interfaces
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_functions(File) ->
	{ok, Src} = epp:parse_file(File, []),
	[Fun#function.name || Fun <- Src, is_record(Fun, function)].

parse(File) ->
	{ok, Src} = epp:parse_file(File, []),
	[Module] = [Mod#attribute.value || Mod = ?attr_module <- Src],
	erlout:set_module_funs([Fun#function.name || Fun <- Src, is_record(Fun, function)]),
	[slide(Fun#function.enrtyes, {Module, Fun#function.name}, Module) || Fun <- Src, is_record(Fun, function)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% New idea to search on tree
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
slide(Element, FunName, Mod) when is_list(Element) ->
	[slide(A, FunName, Mod) || A <- Element],
	ok;

slide(Element, FunName = {Module, Fun}, Mod) when is_tuple(Element) ->
	NextName = case element(1, Element) of
		call ->
			case Element#call.who of
				#atom{} -> 
					erlout:write_link(Module, Fun, Element#call.who#atom.val, Mod),
					{Mod, Element#call.who#atom.val};
				_ -> FunName
			end;
		remote ->
			case {Element#remote.mod, Element#remote.func} of
				{#atom{}, #atom{}} -> 
					erlout:write_link(Module, Fun, Element#remote.mod#atom.val, Element#remote.func#atom.val);
				_ -> FunName
			end;
		_ ->
			FunName
	end,
	[slide(Chpok, NextName, Mod) || Chpok <- tuple_to_list(Element)],
	ok;

slide(_, _, _) -> fuck.
