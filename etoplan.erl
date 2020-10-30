-module(etoplan).

-include("termanus.hrl").

-export([parse/1]).
-export([parself/0]).

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
	
	Functionss = 
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

	[get_calls(Fun#function.name, Fun#function.enrtyes) || Fun <= Functions],
	erlout:finite(File).
		
get_calls(Fun, Entry) ->
	case Entry of 
		?attr_export -> erlout:write_link(File, {Fun, Entry#attribute.value});
		_ -> ok;
	end.