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

%% atomic literals
	
get_calls(#atom{}, Fun) -> ok;
get_calls(#char{}, Fun) -> ok;
get_calls(#float{}, Fun) -> ok;
get_calls(#integer{}, Fun) -> ok;
get_calls(#string{}, Fun) -> ok;
get_calls(#var{}, Fun) -> ok;	
	
%% records
	
get_calls(Entry = #record_index{}, Fun) ->
	get_calls(Entry#record_index.value),
	ok;
	
get_calls(Entry = #record{}, Fun) ->
	get_calls(Entry#record.value),
	ok;

get_calls(?record2, Fun) ->
	get_calls(Val, Fun),
	get_calls(Fields, Fun),
	ok;

get_calls(Entry = #tuple{}, Fun) ->
	get_calls(Entry#tuple.fields, Fun),
	ok;
	
get_calls(Entry = #record_field{}, Fun) ->
	get_calls(Entry#record_field.field, Fun),
	ok;

get_calls(?record_field_exp, Fun) ->
	get_calls(Field, Fun),
	get_calls(Exp, Fun),
	ok;

get_calls(?record_field_wtf, Fun) ->
	get_calls(Field, Fun),
	get_calls(Exp, Fun),
	ok;

get_calls(Entry = #typed_record_field{}, Fun) ->
	get_calls(Entry#typed_record_field.field, Fun),
	get_calls(Entry#typed_record_field.exp, Fun),
	ok;

get_calls(?typed_record_field_exp, Fun) ->
	get_calls(Field, Fun),
	get_calls(Exp, Fun),
	get_calls(TExp, Fun),	
	ok;

%% patterns

get_calls(Entry = #bin{}, Fun) ->
	get_calls(Val, Fun),
	ok;
	
get_calls(Entry = #bin_element{}, Fun) ->
	get_calls(Entry#bin_element.p, Fun),
	get_calls(Entry#bin_element.ssize, Fun),
	get_calls(Entry#bin_element.tsl, Fun),
	ok;

get_calls(Entry = #match{}, Fun) ->
	get_calls(Entry#match.p1, Fun),
	get_calls(Entry#match.p2, Fun),
	ok;
	
get_calls(Entry = #cons{}, Fun) ->
	get_calls(Entry#cons.ph, Fun),
	get_calls(Entry#cons.pt, Fun),
	ok;

get_calls(Entry = #map{}, Fun) ->
	get_calls(Entry#map.entryes, Fun),
	ok;

get_calls(?map_update, Fun) ->
	get_calls(Origin, Fun),
	get_calls(Update, Fun),
	ok;

get_calls(#nil{}, Fun) ->
	ok;
	
get_calls(Entry = #op{}, Fun) ->
	get_calls(Entry#op.value, Fun),
	ok;

get_calls(?op2, Fun) ->
	get_calls(Val1, Fun),
	get_calls(Val2, Fun),
	ok;

get_calls(_, _) -> 
	ok.


