-module(etoplan).

-include("/home/sanya/Рабочий стол/sources/erltoplan/termanus.hrl").

-export([parse/1]).
-export([parself/0]).

-define(output, "output.txt").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Interfaces
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parself() -> parse(?FILE).

parse(File) ->
	erlout:init(?output),
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
		
	erlout:write_export(?output, Extports),
	
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
	[get_calls(Fun#function.enrtyes, Fun#function.name) || Fun <- Functions],
	erlout:finite(?output).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Templates of expressions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
get_calls(Entry, Fun) when is_list(Entry) ->
	[get_calls(Val, Fun) || Val <- Entry],
	ok;
	
get_calls(Entry = #bin{}, Fun) ->
	[get_calls(Val, Fun) || Val <- Entry#bin.elements],
	ok;
	
get_calls(Entry = #bin_element{}, Fun) ->
	get_calls(Entry#bin_element.p, Fun),
	get_calls(Entry#bin_element.ssize, Fun),
	get_calls(Entry#bin_element.tsl, Fun);

get_calls(_, _) -> 
	ok.


