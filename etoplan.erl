-module(etoplan).

-include("/home/sanya/sources/erlang/erltoplan/termanus.hrl").

-export([parse/1]).
-export([parself/0]).

-define(output, "output.txt").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Interfaces
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parself() -> parse(?FILE).

parse(File) ->
	erlout:start(),
	erlout:set_file(?output),
	{ok, Src} = epp:parse_file(File, []),
	
	Extports = 
		proplists:get_keys(
			lists:foldl(
				fun(A, Acc)->
					case A of
						?attr_export -> A#attribute.value ++ Acc;
						_ -> Acc
					end
				end, 
				[], Src
			)
		),
		
	erlout:write_exports(Extports),
	
	Functions = 
		lists:foldl(
			fun(A, Acc)->
				case A of
					#function{} -> [A | Acc];
					_ -> Acc
				end
			end, 
			[], Src
		),
	[slide(Fun#function.enrtyes, Fun#function.name) || Fun <- Functions],
	erlout:finite().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% New idea to search on tree
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
slide(Element, FunName) when is_list(Element) ->
	[slide(A, FunName) || A <- Element];

slide(Element, FunName) when is_tuple(Element) ->
	NextName = case element(1, Element) of
		call ->
			case Element#call.name of
				#atom{} -> 
					Element#call.name#atom.val,
					io:format("\ncall from ~p: \n~p\n", [Element#call.name#atom.val, Element]),
				_ -> FunName
			end;
		remote ->
			case {Element#remote.mod, Element#remote.func} of
				{#atom{}, #atom{}} -> 
					Val = erlout:fart_olink(Element#remote.name#atom.val, Element#remote.func#atom.val),
					io:format("\nremote from ~p: \n~p\n", [Val, Element]),
					Val;
				_ -> FunName
			end;
		_ ->
			ok
	end,
	[slide(Chpok, NextName) || Chpok <- tuple_to_list(Element)];

slide(_, _) -> fuck.
