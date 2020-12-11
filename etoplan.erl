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
	erlout:finite(),
	get_calls(#char{}, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% New idea to search on tree
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
slide(Element, FunName) when is_list(Element) ->
	[slide(A, FunName) || A <- Element];

slide(Element, FunName) when is_tuple(Element) ->
	case element(1, Element) of
		call ->
			io:format("\nfounded call: \n~p\n", [Element]);
		remote ->
			io:format("\nfounded rem: \n~p\n", [Element]);
		_ ->
			ok
	end,
	[slide(Chpok, FunName) || Chpok <- tuple_to_list(Element)];

slide(_, _) -> fuck.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Templates of expressions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
get_calls(Entry, Fun) when is_list(Entry) ->
	[get_calls(Val, Fun) || Val <- Entry],
	ok;

%% atomic literals
	
get_calls(Entry = #atom{}, Fun) -> erlout:write_link(Fun, Entry#atom.val);
get_calls(#char{}, Fun) -> ok;
get_calls(#float{}, Fun) -> ok;
get_calls(#integer{}, Fun) -> ok;
get_calls(#string{}, Fun) -> ok;
get_calls(#var{}, Fun) -> ok;	
	
%% records
	
get_calls(Entry = #record_index{}, Fun) ->
	get_calls(Entry#record_index.value, Fun),
	ok;
	
get_calls(Entry = #record{}, Fun) ->
	get_calls(Entry#record.value, Fun),
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

get_calls(?typed_record_field_exp, Fun) ->
	get_calls(Field, Fun),
	get_calls(Exp, Fun),
	get_calls(TExp, Fun),	
	ok;

get_calls(Entry = #typed_record_field{}, Fun) ->
	get_calls(Entry#typed_record_field.field, Fun),
	get_calls(Entry#typed_record_field.exp, Fun),
	ok;

%% patterns

get_calls(Entry = #bin{}, Fun) ->
	get_calls(Entry#bin.elements, Fun),
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

%% Expressions

get_calls(Entry = #bc{}, Fun) ->
	get_calls(Entry#bc.e0, Fun),
	get_calls(Entry#bc.q, Fun),
	ok;

get_calls(Entry = #block{}, Fun) ->
	get_calls(Entry#block.val, Fun),
	ok;

get_calls(Entry = #'case'{}, Fun) ->
	get_calls(Entry#'case'.val, Fun),
	ok;

get_calls(Entry = #'catch'{}, Fun) ->
	get_calls(Entry#'catch'.val, Fun),
	ok;

get_calls(Entry = #'if'{}, Fun) ->
	get_calls(Entry#'if'.val, Fun),
	ok;

get_calls(Entry = #'lc'{}, Fun) ->
	get_calls(Entry#'lc'.val, Fun),
	ok;

get_calls(Entry = #'receive'{}, Fun) ->
	get_calls(Entry#'receive'.val, Fun),
	ok;

get_calls(?receive_after, Fun) ->
	get_calls(Cond, Fun),
	get_calls(While, Fun),
	get_calls(After, Fun),
	ok;

get_calls(Entry = #'try'{}, Fun) ->
	get_calls(Entry#'try'.body, Fun),
	get_calls(Entry#'try'.exp, Fun),
	get_calls(Entry#'try'.val1, Fun),
	get_calls(Entry#'try'.val2, Fun),
	ok;

%% functions

%% Qualifiers

get_calls(Entry = #generate{}, Fun) ->
	get_calls(Entry#generate.from, Fun),
	get_calls(Entry#generate.to, Fun),
	ok;

get_calls(Entry = #b_generate{}, Fun) ->
	get_calls(Entry#b_generate.from, Fun),
	get_calls(Entry#b_generate.to, Fun),
	ok;

%% Associations

get_calls(Entry = #map_field_assoc{}, Fun) ->
	get_calls(Entry#map_field_assoc.what, Fun),
	get_calls(Entry#map_field_assoc.update, Fun),
	ok;

%% 8.5  Clauses

get_calls(Entry = #clause{}, Fun) ->
	get_calls(Entry#clause.head, Fun),
	get_calls(Entry#clause.body, Fun),
	get_calls(Entry#clause.tail, Fun),
	ok;

get_calls(Entry = #throw{}, Fun) ->
	get_calls(Entry#throw.any, Fun),
	ok;

%% 8.7  Types

get_calls(Entry = #ann_type{}, Fun) ->
	get_calls(Entry#ann_type.value, Fun),
	ok;

get_calls(Entry = #remote_type{}, Fun) ->
	get_calls(Entry#remote_type.value, Fun),
	ok;

get_calls(Entry = #type{}, Fun) ->
	get_calls(Entry#type.value, Fun),
	ok;

%% else

get_calls(_, _) -> 
	ok.


