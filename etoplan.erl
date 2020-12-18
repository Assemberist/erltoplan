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

	erlout:shade_functions([ Fun || {Fun, _} <- erlang:module_info(functions)]),
	erlout:shade_modules([lists, mnesia, erlang, proplists, gen_server]),
	
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
