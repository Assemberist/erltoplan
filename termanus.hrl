-record(function, {anno, name, arity, enrtyes}). %% {function,ANNO,Name,Arity,[Rep(Fc_1)]}.

%% attributes
-record(attribute, {anno, type, value}).
-define(attr_export, #attribute{type = export}). 	%% {attribute,ANNO,export,[{Fun_1,A_1}]}.
-define(attr_import, #attribute{type = import}). 	%% {attribute,ANNO,import,{Mod,[{Fun_1,A_1}]}}.
-define(attr_module, #attribute{type = module}). 	%% {attribute,ANNO,module,Mod}.
-define(attr_file, #attribute{type = file}). 		%% {attribute,ANNO,file,{File,Line}}.
-define(attr_spec, #attribute{type = spec}). 		%% {attribute,ANNO,spec,{{Mod,Name,Arity},[Rep(Ft_1)]}}.
-define(attr_record, #attribute{type = record}). 	%% {attribute,ANNO,record,{Name,[Rep(V_1)]}}.
													%% {attribute,ANNO,Spec,{{Name,Arity},[Rep(Ft_1)]}}.
													%% {attribute,ANNO,Type,{Name,Rep(T),[Rep(V_1)]}}.
													%% {attribute,ANNO,A,T}.

%% record fields
-record(record_index, {anno, name, value}). 									%% {record_index,ANNO,Name,Rep(Field)}.
-record(record, {anno, name, value}). 											%% {record,ANNO,Name,[{record_field,ANNO,Rep(Field_1),Rep(P_1)}]}.
-define(record2, {record, ANNO, Val, RecName, Fields}).							%% {record,ANNO,Rep(E_0),Name,[{record_field,ANNO,Rep(Field_1),Rep(E_1)}]}.
-record(tuple, {anno, fields}). 												%% {tuple,ANNO,[Rep(P_1)]}.
-record(record_field, {anno, field}). 											%% {record_field,ANNO,Rep(A)}.
-define(record_field_exp, {record_field, ANNO, Field, Exp}).					%% {record_field,ANNO,Rep(A),Rep(E)}.
-define(record_field_wtf, {record_field, ANNO, Field, Name, Exp}).				%% {record_field,ANNO,Rep(Gt_0),Name,Rep(Field)}.
-record(typed_record_field, {field :: #record_field{}, exp}). 					%% {typed_record_field,{record_field,ANNO,Rep(A)},Rep(T)}.
-define(typed_record_field_exp, {typed_record_field, ?record_field_exp, TExp}).	%% {record,ANNO,Name,[{record_field,ANNO,Rep(Field_1),Rep(Gt_1)}]}.


%% atomic literals
-define(atomic(A), -record(A, {anno, val})).
?atomic(atom). 		%% {atom,ANNO,L}.
?atomic(char). 		%% {char,ANNO,L}.
?atomic(float). 	%% {float,ANNO,L}.
?atomic(integer). 	%% {integer,ANNO,L}.
?atomic(string). 	%% {string,ANNO,[C_1]}.
?atomic(var). 		%% {var, ANNO, V}.

%% patterns
-record(bin_element, {anno, p, ssize, tsl}). 		%% {bin_element,ANNO,Rep(P_1),Rep(Size_1),Rep(TSL_1)}
-record(bin, {anno, elements :: [#bin_element{}]}). %% {bin,ANNO,[{bin_element,ANNO,Rep(P_k),Rep(Size_k),Rep(TSL_k)}]}. 
-record(match, {anno, p1, p2}).						%% {match,ANNO,Rep(P_1),Rep(P_2)}
-record(cons, {anno, ph, pt}).						%% {cons,ANNO,Rep(P_h),Rep(P_t)}.
-record(map, {anno, entryes}).						%% {map,ANNO,[Rep(A_1)]}.
-define(map_update, {map, ANNO, Origin, Update}). 	%% {map,ANNO,Rep(E_0),[Rep(A_1)]}.
-record(nil, {anno}).								%% {nil,ANNO}.
-record(op, {anno, oper, value}).					%% {op,ANNO,Op,Rep(Gt_0)}.
-define(op2, {op, ANNO, Op, Val1, Val2}). 			%% {op,ANNO,Op,Rep(P_1),Rep(P_2)}.

%% Expressions
-record(bc, {anno, e0, q}). 						%% {bc,ANNO,Rep(E_0),[Rep(Q_1)]}.
-record(block, {anno, val}). 						%% {block,ANNO,Rep(B)}.
-record('case', {anno, val}). 						%% {'case',ANNO,Rep(E_0),[Rep(Cc_1)]}.
-record('catch', {anno, val}). 						%% {'catch',ANNO,Rep(E_0)}.
-record('if', {anno, val}). 						%% {'if',ANNO,[Rep(Ic_1)]}.
-record('lc', {anno, val}). 						%% {'lc',ANNO,[Rep(Ic_1)]}.
-record('receive', {anno, val}). 					%% {'receive',ANNO,[Rep(Cc_1)]}.
-define(receive_after, {'receive', ANNO, Cond, While, After}).	%% {'receive',ANNO,[Rep(Cc_1)],Rep(E_0),Rep(B_t)}.
-record('try', {anno, body, exp, val1, val2}). 	%% {'try',ANNO,Rep(B),[Rep(Cc_1)],[Rep(Tc_1)],Rep(A)}.

%% functions
-define(func, {function, _, _}).
-define(func_full, {function, _, _, _}).
-define(func_local, {'fun', _, ?func}). 		%% {'fun',ANNO,{function,Name,Arity}}.
-define(func_far, {'fun', _, ?func_full). 		%% {'fun',ANNO,{function,Rep(Module),Rep(Name),Rep(Arity)}}.
-define(func_claus, {'fun', _, {clauses, _}}). 	%% {'fun',ANNO,{clauses,[Rep(Fc_1)]}}.
-record(named_fun, {anno, name, value}). 		%% {named_fun,ANNO,Name,[Rep(Fc_1)]}.
-record(call, {anno, who, value}). 				%% {call,ANNO,Rep(E_0),[Rep(E_1)]}.
-record(remote, {anno, mod, func}). 			%% {call,ANNO,{remote,ANNO,Rep(E_m),Rep(E_0)},[Rep(E_1)]}.

%% Qualifiers
-record(generate, {anno, from, to}). 	%% {generate,ANNO,Rep(P),Rep(E)}.
-record(b_generate, {anno, from, to}). 	%% {generate,ANNO,Rep(P),Rep(E)}.

%% Associations
-record(map_field_assoc, {anno, what, update}).

%% 8.5  Clauses
-record(clause, {anno, head, body, tail}). 	%% {clause,ANNO,[Rep(P)],Rep(Gs),Rep(B)}.
-record(throw, {val, any}). 				%% {clause,ANNO,[Rep({throw,P,_})],[],Rep(B)}
											%% {clause,ANNO,[Rep({X,P,_})],[],Rep(B)}
											%% {clause,ANNO,[Rep({X,P,S})],[],Rep(B)}.
											%% {clause,ANNO,[Rep({throw,P,_})],Rep(Gs),Rep(B)}
											%% {clause,ANNO,[Rep({X,P,_})],Rep(Gs),Rep(B)}
											%% {clause,ANNO,[Rep({X,P,S})],Rep(Gs),Rep(B)}.
											%% {clause,ANNO,Rep(Ps),[],Rep(B)}.
											%% {clause,ANNO,Rep(Ps),Rep(Gs),Rep(B)}.
											%% {clause,ANNO,[],Rep(Gs),Rep(B)}.

%% 8.7  Types
-record(ann_type, {anno, value}). 		%% {ann_type,ANNO,[Rep(A),Rep(T_0)]}.
-record(remote_type, {anno, value}). 	%% {remote_type,ANNO,[Rep(M),Rep(N),[Rep(T_1)]]}.
-record(type, {anno, wtf, value}). 		%% {user_type,ANNO,N,[Rep(T_1)]}.
%% {type,ANNO,binary,[Rep(M),Rep(N)]}.
%% {type,ANNO,nil,[]}
%% {type,ANNO,'fun',[]}.
%% {type,ANNO,'fun',[{type,ANNO,any},Rep(T_0)]}.
%% {type,ANNO,'fun',[{type,ANNO,product,[Rep(T_1)]},Rep(T_0)]}.
%% {type,ANNO,range,[Rep(L),Rep(H)]}.
%% {type,ANNO,map,any}.
%% {type,ANNO,map,[Rep(A_1)]}.
%% {type,ANNO,N,[Rep(T_1)]}.
%% {type,ANNO,record,[Rep(Name),Rep(F_1)]}. 
%% {type,ANNO,tuple,any}.
%% {type,ANNO,tuple,[Rep(T_1)]}.
%% {type,ANNO,union,[Rep(T_1)]}.
%% {type,ANNO,bounded_fun,[Rep(Ft_1),Rep(Fc)]}.
%% {type,ANNO,constraint,[{atom,ANNO,is_subtype},[Rep(V),Rep(T)]]}.
%% {type,ANNO,map_field_assoc,[Rep(K),Rep(V)]}.
%% {type,ANNO,map_field_exact,[Rep(K),Rep(V)]}.
%% {type,ANNO,field_type,[Rep(Name),Rep(Type)]}.
