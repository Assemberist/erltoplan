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
	[slide(Fun#function.enrtyes, Fun#function.name, Module) || Fun <- Src, is_record(Fun, function)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% New idea to search on tree
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
slide(Element, FunName, Mod) when is_list(Element) ->
	[slide(A, FunName, Mod) || A <- Element],
	ok;

slide(Element, FunName, Mod) when is_tuple(Element) ->
	NextName = case element(1, Element) of
		call ->
			case Element#call.who of
				#atom{} -> 
					Link = erlout:fart_olink(Mod, FunName),
					erlout:write_link(Link, Element#call.who#atom.val),
					Element#call.who#atom.val;
				_ -> FunName
			end;
		remote ->
			case {Element#remote.mod, Element#remote.func} of
				{#atom{}, #atom{}} -> 
					Link = erlout:fart_olink(Mod, FunName),
					erlout:write_far_link(Link, Element#remote.mod#atom.val, Element#remote.func#atom.val),
					erlout:fart_olink(Element#remote.mod#atom.val, Element#remote.func#atom.val);
				_ -> FunName
			end;
		_ ->
			FunName
	end,
	[slide(Chpok, NextName, Mod) || Chpok <- tuple_to_list(Element)],
	ok;

slide(_, _, _) -> fuck.
