-module(etoplan).

-export([parse/1]).
-export([parself/0]).

parself() -> parse(?FILE).

parse(File) ->
	{ok, Src} = epp:parse_file(File, []),
	
	Extports = 
		proplists:get_keys(
			lists:foldl(
				fun(A, Acc)->
					case A of
						{attribute, _, export, FunList} -> FunList ++ Acc;
						_ -> Acc
					end
				end, 
				[], Src
			)
		),
		
	{ok, Src}.
		
	