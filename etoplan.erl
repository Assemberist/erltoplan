-module(etoplan).

-include("/home/sanya/sources/erlang/erltoplan/termanus.hrl").

-export([parse/1]).

-define(output(File), File ++ ".txt").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Interfaces
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse(File) ->
	erlout:set_file(?output(File)),
	{ok, Src} = epp:parse_file(File, []),
	
	erlout:write_exports([Exp#attribute.value || Exp <- Src, Exp#attribute.type == export]),
	
	[slide(Fun#function.enrtyes, Fun#function.name) || Fun <- Src, is_record(Fun, function)],
	erlout:finite().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% New idea to search on tree
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
slide(Element, FunName) when is_list(Element) ->
	[slide(A, FunName) || A <- Element],
	ok;

slide(Element, FunName) when is_tuple(Element) ->
	NextName = case element(1, Element) of
		call ->
			case Element#call.who of
				#atom{} -> 
					erlout:write_link(FunName, Element#call.who#atom.val),
					Element#call.who#atom.val;
				_ -> FunName
			end;
		remote ->
			case {Element#remote.mod, Element#remote.func} of
				{#atom{}, #atom{}} -> 
					erlout:write_far_link(FunName, Element#remote.mod#atom.val, Element#remote.func#atom.val),
					erlout:fart_olink(Element#remote.mod#atom.val, Element#remote.func#atom.val);
				_ -> FunName
			end;
		_ ->
			FunName
	end,
	[slide(Chpok, NextName) || Chpok <- tuple_to_list(Element)],
	ok;

slide(_, _) -> fuck.
