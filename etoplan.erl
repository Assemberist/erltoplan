-module(etoplan).

-include("termanus.hrl").

-export([parse/1]).
-export([parself/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Interfaces
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parself() -> parse(?FILE).

parse(File) ->
	erlout:init(File),
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
		
	erlout:write_export(File, Extports),
	
	Functions = 
		proplists:get_keys(
			lists:foldl(
				fun(A, Acc)->
					case A of
						#function{} -> A ++ Acc;
						_ -> Acc
					end
				end, 
				[], Src
			)
		),

	[get_calls(Fun#function.enrtyes, Fun#function.name) || Fun <- Functions],
	erlout:finite(File).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Templates of expressions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
get_calls(Entry, Fun) when is_list(Entry) ->
	lists:foldl(get_calls, Fun, Entry);
	
get_calls(Entry = #bin{}, Fun) ->
	lists:foldl(get_calls, Fun, Entry#bin.elements);
	
get_calls(Entry = #bin_element{}, Fun) ->
	get_calls(Entry#bin_element.p, Fun),
	get_calls(Entry#bin_element.ssize, Fun),
	get_calls(Entry#bin_element.tsl, Fun).


