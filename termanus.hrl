-record(function, {anno, name, arity, enrtyes}). %% {function,ANNO,Name,Arity,[Rep(Fc_1), ...,Rep(Fc_k)]}.

%% attributes
-record(attribute, {anno, type, value}).
-define(attr_export, #attribute{type = export}). 	%% {attribute,ANNO,export,[{Fun_1,A_1}, ..., {Fun_k,A_k}]}.
-define(attr_import, #attribute{type = import}). 	%% {attribute,ANNO,import,{Mod,[{Fun_1,A_1}, ..., {Fun_k,A_k}]}}.
-define(attr_module, #attribute{type = module}). 	%% {attribute,ANNO,module,Mod}.
-define(attr_file, #attribute{type = file}). 		%% {attribute,ANNO,file,{File,Line}}.
-define(attr_spec, #attribute{type = spec}). 		%% {attribute,ANNO,spec,{{Mod,Name,Arity},[Rep(Ft_1), ..., Rep(Ft_k)]}}.
-define(attr_record, #attribute{type = record}). 	%% {attribute,ANNO,record,{Name,[Rep(V_1), ..., Rep(V_k)]}}.
-define(attr_spec, #attribute{type = spec}). 		%% {attribute,ANNO,spec,{{Mod,Name,Arity},[Rep(Ft_1), ..., Rep(Ft_k)]}}.
													%% {attribute,ANNO,Spec,{{Name,Arity},[Rep(Ft_1), ..., Rep(Ft_k)]}}.
													%% {attribute,ANNO,Type,{Name,Rep(T),[Rep(V_1), ..., Rep(V_k)]}}.
													%% {attribute,ANNO,A,T}.

%% record fields
-record(record_field, {anno, term}). %% {record_field,ANNO,Rep(A)}.
-record(record_field_exp, anno, lval, rval}). %% {record_field,ANNO,Rep(A),Rep(E)}.
-record(typed_record_field, {field :: record_field, type}). %% {typed_record_field,{record_field,ANNO,Rep(A)},Rep(T)}.
-record(typed_record_field_exp, {field_exp :: record_field_exp, type}). %% {typed_record_field,{record_field,ANNO,Rep(A),Rep(E)},Rep(T)}.

%% atomic literals
-define(atomc(A), -record(A, {anno, val})).
?atomic(atom).
?atomic(char).
?atomic(float).
?atomic(integer).
?atomic(string).
%% -record(atom, {anno, val}). 					%% {atom,ANNO,L}.
%% -record(char, {anno, val}). 					%% {char,ANNO,L}.
%% -record(float, {anno, val}). 					%% {float,ANNO,L}.
%% -record(integer, {anno, val}). 					%% {integer,ANNO,L}.
%% -record(string, {anno, val}). 					%% {string,ANNO,[C_1, ..., C_k]}.

%% patterns
-record(bin_element, {anno, p, size, tsl}). 	%% {bin_element,ANNO,Rep(P_1),Rep(Size_1),Rep(TSL_1)}
-record(bin, {anno, element :: bin_element}). 	%% {bin,ANNO,[{bin_element,ANNO,Rep(P_k),Rep(Size_k),Rep(TSL_k)}]}. 
-record(match, {anno, p1, p2}).					%% {match,ANNO,Rep(P_1),Rep(P_2)}
-record(cons, {anno, ph, pt}).					%% {cons,ANNO,Rep(P_h),Rep(P_t)}.
-record(map, {anno, entryes}).					%% {map,ANNO,[Rep(A_1), ..., Rep(A_k)]}.
-record(nil, {anno}).							%% {nil,ANNO}.
-record(op, {anno, operator, lval, rval}). 		%% {op,ANNO,Op,Rep(P_1),Rep(P_2)}.
{op,ANNO,Op,Rep(P_0)}.


-record(op, {anno, }).

If P is an operator pattern Op P_0, where Op is a unary operator (this is an occurrence of an expression that can be evaluated to a number at compile time), then Rep(P) = {op,ANNO,Op,Rep(P_0)}.

If P is a parenthesized pattern ( P_0 ), then Rep(P) = Rep(P_0), that is, parenthesized patterns cannot be distinguished from their bodies.

If P is a record field index pattern #Name.Field, where Field is an atom, then Rep(P) = {record_index,ANNO,Name,Rep(Field)}.

If P is a record pattern #Name{Field_1=P_1, ..., Field_k=P_k}, where each Field_i is an atom or _, then Rep(P) = {record,ANNO,Name,[{record_field,ANNO,Rep(Field_1),Rep(P_1)}, ..., {record_field,ANNO,Rep(Field_k),Rep(P_k)}]}.

If P is a tuple pattern {P_1, ..., P_k}, then Rep(P) = {tuple,ANNO,[Rep(P_1), ..., Rep(P_k)]}.

If P is a universal pattern _, then Rep(P) = {var,ANNO,'_'}.

If P is a variable pattern V, then Rep(P) = {var,ANNO,A}, where A is an atom with a printname consisting of the same characters as V.

Notice that every pattern has the same source form as some expression, and is represented in the same way as the corresponding expression.


8.4  Expressions
A body B is a non-empty sequence of expressions E_1, ..., E_k, and Rep(B) = [Rep(E_1), ..., Rep(E_k)].

An expression E is one of the following:

If E is an atomic literal L, then Rep(E) = Rep(L).

If E is a bitstring comprehension <<E_0 || Q_1, ..., Q_k>>, where each Q_i is a qualifier, then Rep(E) = {bc,ANNO,Rep(E_0),[Rep(Q_1), ..., Rep(Q_k)]}. For Rep(Q), see below.

If E is a bitstring constructor <<E_1:Size_1/TSL_1, ..., E_k:Size_k/TSL_k>>, where each Size_i is an expression and each TSL_i is a type specificer list, then Rep(E) = {bin,ANNO,[{bin_element,ANNO,Rep(E_1),Rep(Size_1),Rep(TSL_1)}, ..., {bin_element,ANNO,Rep(E_k),Rep(Size_k),Rep(TSL_k)}]}. For Rep(TSL), see below. An omitted Size_i is represented by default. An omitted TSL_i is represented by default.

If E is a block expression begin B end, where B is a body, then Rep(E) = {block,ANNO,Rep(B)}.

If E is a case expression case E_0 of Cc_1 ; ... ; Cc_k end, where E_0 is an expression and each Cc_i is a case clause, then Rep(E) = {'case',ANNO,Rep(E_0),[Rep(Cc_1), ..., Rep(Cc_k)]}.

If E is a catch expression catch E_0, then Rep(E) = {'catch',ANNO,Rep(E_0)}.

If E is a cons skeleton [E_h | E_t], then Rep(E) = {cons,ANNO,Rep(E_h),Rep(E_t)}.

If E is a fun expression fun Name/Arity, then Rep(E) = {'fun',ANNO,{function,Name,Arity}}.

If E is a fun expression fun Module:Name/Arity, then Rep(E) = {'fun',ANNO,{function,Rep(Module),Rep(Name),Rep(Arity)}}. (Before Erlang/OTP R15: Rep(E) = {'fun',ANNO,{function,Module,Name,Arity}}.)

If E is a fun expression fun Fc_1 ; ... ; Fc_k end, where each Fc_i is a function clause, then Rep(E) = {'fun',ANNO,{clauses,[Rep(Fc_1), ..., Rep(Fc_k)]}}.

If E is a fun expression fun Name Fc_1 ; ... ; Name Fc_k end, where Name is a variable and each Fc_i is a function clause, then Rep(E) = {named_fun,ANNO,Name,[Rep(Fc_1), ..., Rep(Fc_k)]}.

If E is a function call E_0(E_1, ..., E_k), then Rep(E) = {call,ANNO,Rep(E_0),[Rep(E_1), ..., Rep(E_k)]}.

If E is a function call E_m:E_0(E_1, ..., E_k), then Rep(E) = {call,ANNO,{remote,ANNO,Rep(E_m),Rep(E_0)},[Rep(E_1), ..., Rep(E_k)]}.

If E is an if expression if Ic_1 ; ... ; Ic_k end, where each Ic_i is an if clause, then Rep(E) = {'if',ANNO,[Rep(Ic_1), ..., Rep(Ic_k)]}.

If E is a list comprehension [E_0 || Q_1, ..., Q_k], where each Q_i is a qualifier, then Rep(E) = {lc,ANNO,Rep(E_0),[Rep(Q_1), ..., Rep(Q_k)]}. For Rep(Q), see below.

If E is a map creation #{A_1, ..., A_k}, where each A_i is an association E_i_1 => E_i_2, then Rep(E) = {map,ANNO,[Rep(A_1), ..., Rep(A_k)]}. For Rep(A), see below.

If E is a map update E_0#{A_1, ..., A_k}, where each A_i is an association E_i_1 => E_i_2 or E_i_1 := E_i_2, then Rep(E) = {map,ANNO,Rep(E_0),[Rep(A_1), ..., Rep(A_k)]}. For Rep(A), see below.

If E is a match operator expression P = E_0, where P is a pattern, then Rep(E) = {match,ANNO,Rep(P),Rep(E_0)}.

If E is nil, [], then Rep(E) = {nil,ANNO}.

If E is an operator expression E_1 Op E_2, where Op is a binary operator other than match operator =, then Rep(E) = {op,ANNO,Op,Rep(E_1),Rep(E_2)}.

If E is an operator expression Op E_0, where Op is a unary operator, then Rep(E) = {op,ANNO,Op,Rep(E_0)}.

If E is a parenthesized expression ( E_0 ), then Rep(E) = Rep(E_0), that is, parenthesized expressions cannot be distinguished from their bodies.

If E is a receive expression receive Cc_1 ; ... ; Cc_k end, where each Cc_i is a case clause, then Rep(E) = {'receive',ANNO,[Rep(Cc_1), ..., Rep(Cc_k)]}.

If E is a receive expression receive Cc_1 ; ... ; Cc_k after E_0 -> B_t end, where each Cc_i is a case clause, E_0 is an expression, and B_t is a body, then Rep(E) = {'receive',ANNO,[Rep(Cc_1), ..., Rep(Cc_k)],Rep(E_0),Rep(B_t)}.

If E is a record creation #Name{Field_1=E_1, ..., Field_k=E_k}, where each Field_i is an atom or _, then Rep(E) = {record,ANNO,Name,[{record_field,ANNO,Rep(Field_1),Rep(E_1)}, ..., {record_field,ANNO,Rep(Field_k),Rep(E_k)}]}.

If E is a record field access E_0#Name.Field, where Field is an atom, then Rep(E) = {record_field,ANNO,Rep(E_0),Name,Rep(Field)}.

If E is a record field index #Name.Field, where Field is an atom, then Rep(E) = {record_index,ANNO,Name,Rep(Field)}.

If E is a record update E_0#Name{Field_1=E_1, ..., Field_k=E_k}, where each Field_i is an atom, then Rep(E) = {record,ANNO,Rep(E_0),Name,[{record_field,ANNO,Rep(Field_1),Rep(E_1)}, ..., {record_field,ANNO,Rep(Field_k),Rep(E_k)}]}.

If E is a tuple skeleton {E_1, ..., E_k}, then Rep(E) = {tuple,ANNO,[Rep(E_1), ..., Rep(E_k)]}.

If E is a try expression try B catch Tc_1 ; ... ; Tc_k end, where B is a body and each Tc_i is a catch clause, then Rep(E) = {'try',ANNO,Rep(B),[],[Rep(Tc_1), ..., Rep(Tc_k)],[]}.

If E is a try expression try B of Cc_1 ; ... ; Cc_k catch Tc_1 ; ... ; Tc_n end, where B is a body, each Cc_i is a case clause, and each Tc_j is a catch clause, then Rep(E) = {'try',ANNO,Rep(B),[Rep(Cc_1), ..., Rep(Cc_k)],[Rep(Tc_1), ..., Rep(Tc_n)],[]}.

If E is a try expression try B after A end, where B and A are bodies, then Rep(E) = {'try',ANNO,Rep(B),[],[],Rep(A)}.

If E is a try expression try B of Cc_1 ; ... ; Cc_k after A end, where B and A are a bodies, and each Cc_i is a case clause, then Rep(E) = {'try',ANNO,Rep(B),[Rep(Cc_1), ..., Rep(Cc_k)],[],Rep(A)}.

If E is a try expression try B catch Tc_1 ; ... ; Tc_k after A end, where B and A are bodies, and each Tc_i is a catch clause, then Rep(E) = {'try',ANNO,Rep(B),[],[Rep(Tc_1), ..., Rep(Tc_k)],Rep(A)}.

If E is a try expression try B of Cc_1 ; ... ; Cc_k catch Tc_1 ; ... ; Tc_n after A end, where B and A are a bodies, each Cc_i is a case clause, and each Tc_j is a catch clause, then Rep(E) = {'try',ANNO,Rep(B),[Rep(Cc_1), ..., Rep(Cc_k)],[Rep(Tc_1), ..., Rep(Tc_n)],Rep(A)}.

If E is a variable V, then Rep(E) = {var,ANNO,A}, where A is an atom with a printname consisting of the same characters as V.

Qualifiers
A qualifier Q is one of the following:

If Q is a filter E, where E is an expression, then Rep(Q) = Rep(E).

If Q is a generator P <- E, where P is a pattern and E is an expression, then Rep(Q) = {generate,ANNO,Rep(P),Rep(E)}.

If Q is a bitstring generator P <= E, where P is a pattern and E is an expression, then Rep(Q) = {b_generate,ANNO,Rep(P),Rep(E)}.

Bitstring Element Type Specifiers
A type specifier list TSL for a bitstring element is a sequence of type specifiers TS_1 - ... - TS_k, and Rep(TSL) = [Rep(TS_1), ..., Rep(TS_k)].

If TS is a type specifier A, where A is an atom, then Rep(TS) = A.

If TS is a type specifier A:Value, where A is an atom and Value is an integer, then Rep(TS) = {A,Value}.

Associations
An association A is one of the following:

If A is an association K => V, then Rep(A) = {map_field_assoc,ANNO,Rep(K),Rep(V)}.

If A is an association K := V, then Rep(A) = {map_field_exact,ANNO,Rep(K),Rep(V)}.

8.5  Clauses
There are function clauses, if clauses, case clauses, and catch clauses.

A clause C is one of the following:

If C is a case clause P -> B, where P is a pattern and B is a body, then Rep(C) = {clause,ANNO,[Rep(P)],[],Rep(B)}.

If C is a case clause P when Gs -> B, where P is a pattern, Gs is a guard sequence, and B is a body, then Rep(C) = {clause,ANNO,[Rep(P)],Rep(Gs),Rep(B)}.

If C is a catch clause P -> B, where P is a pattern and B is a body, then Rep(C) = {clause,ANNO,[Rep({throw,P,_})],[],Rep(B)}, that is, a catch clause with an explicit exception class throw and with or without an explicit stacktrace variable _ cannot be distinguished from a catch clause without an explicit exception class and without an explicit stacktrace variable.

If C is a catch clause X : P -> B, where X is an atomic literal or a variable pattern, P is a pattern, and B is a body, then Rep(C) = {clause,ANNO,[Rep({X,P,_})],[],Rep(B)}, that is, a catch clause with an explicit exception class and with an explicit stacktrace variable _ cannot be distinguished from a catch clause with an explicit exception class and without an explicit stacktrace variable.

If C is a catch clause X : P : S -> B, where X is an atomic literal or a variable pattern, P is a pattern, S is a variable, and B is a body, then Rep(C) = {clause,ANNO,[Rep({X,P,S})],[],Rep(B)}.

If C is a catch clause P when Gs -> B, where P is a pattern, Gs is a guard sequence, and B is a body, then Rep(C) = {clause,ANNO,[Rep({throw,P,_})],Rep(Gs),Rep(B)}, that is, a catch clause with an explicit exception class throw and with or without an explicit stacktrace variable _ cannot be distinguished from a catch clause without an explicit exception class and without an explicit stacktrace variable.

If C is a catch clause X : P when Gs -> B, where X is an atomic literal or a variable pattern, P is a pattern, Gs is a guard sequence, and B is a body, then Rep(C) = {clause,ANNO,[Rep({X,P,_})],Rep(Gs),Rep(B)}, that is, a catch clause with an explicit exception class and with an explicit stacktrace variable _ cannot be distinguished from a catch clause with an explicit exception class and without an explicit stacktrace variable.

If C is a catch clause X : P : S when Gs -> B, where X is an atomic literal or a variable pattern, P is a pattern, Gs is a guard sequence, S is a variable, and B is a body, then Rep(C) = {clause,ANNO,[Rep({X,P,S})],Rep(Gs),Rep(B)}.

If C is a function clause ( Ps ) -> B, where Ps is a pattern sequence and B is a body, then Rep(C) = {clause,ANNO,Rep(Ps),[],Rep(B)}.

If C is a function clause ( Ps ) when Gs -> B, where Ps is a pattern sequence, Gs is a guard sequence and B is a body, then Rep(C) = {clause,ANNO,Rep(Ps),Rep(Gs),Rep(B)}.

If C is an if clause Gs -> B, where Gs is a guard sequence and B is a body, then Rep(C) = {clause,ANNO,[],Rep(Gs),Rep(B)}.

8.6  Guards
A guard sequence Gs is a sequence of guards G_1; ...; G_k, and Rep(Gs) = [Rep(G_1), ..., Rep(G_k)]. If the guard sequence is empty, then Rep(Gs) = [].

A guard G is a non-empty sequence of guard tests Gt_1, ..., Gt_k, and Rep(G) = [Rep(Gt_1), ..., Rep(Gt_k)].

A guard test Gt is one of the following:

If Gt is an atomic literal L, then Rep(Gt) = Rep(L).

If Gt is a bitstring constructor <<Gt_1:Size_1/TSL_1, ..., Gt_k:Size_k/TSL_k>>, where each Size_i is a guard test and each TSL_i is a type specificer list, then Rep(Gt) = {bin,ANNO,[{bin_element,ANNO,Rep(Gt_1),Rep(Size_1),Rep(TSL_1)}, ..., {bin_element,ANNO,Rep(Gt_k),Rep(Size_k),Rep(TSL_k)}]}. For Rep(TSL), see above. An omitted Size_i is represented by default. An omitted TSL_i is represented by default.

If Gt is a cons skeleton [Gt_h | Gt_t], then Rep(Gt) = {cons,ANNO,Rep(Gt_h),Rep(Gt_t)}.

If Gt is a function call A(Gt_1, ..., Gt_k), where A is an atom, then Rep(Gt) = {call,ANNO,Rep(A),[Rep(Gt_1), ..., Rep(Gt_k)]}.

If Gt is a function call A_m:A(Gt_1, ..., Gt_k), where A_m is the atom erlang and A is an atom or an operator, then Rep(Gt) = {call,ANNO,{remote,ANNO,Rep(A_m),Rep(A)},[Rep(Gt_1), ..., Rep(Gt_k)]}.

If Gt is a map creation #{A_1, ..., A_k}, where each A_i is an association Gt_i_1 => Gt_i_2, then Rep(Gt) = {map,ANNO,[Rep(A_1), ..., Rep(A_k)]}. For Rep(A), see above.

If Gt is a map update Gt_0#{A_1, ..., A_k}, where each A_i is an association Gt_i_1 => Gt_i_2 or Gt_i_1 := Gt_i_2, then Rep(Gt) = {map,ANNO,Rep(Gt_0),[Rep(A_1), ..., Rep(A_k)]}. For Rep(A), see above.

If Gt is nil, [], then Rep(Gt) = {nil,ANNO}.

If Gt is an operator guard test Gt_1 Op Gt_2, where Op is a binary operator other than match operator =, then Rep(Gt) = {op,ANNO,Op,Rep(Gt_1),Rep(Gt_2)}.

If Gt is an operator guard test Op Gt_0, where Op is a unary operator, then Rep(Gt) = {op,ANNO,Op,Rep(Gt_0)}.

If Gt is a parenthesized guard test ( Gt_0 ), then Rep(Gt) = Rep(Gt_0), that is, parenthesized guard tests cannot be distinguished from their bodies.

If Gt is a record creation #Name{Field_1=Gt_1, ..., Field_k=Gt_k}, where each Field_i is an atom or _, then Rep(Gt) = {record,ANNO,Name,[{record_field,ANNO,Rep(Field_1),Rep(Gt_1)}, ..., {record_field,ANNO,Rep(Field_k),Rep(Gt_k)}]}.

If Gt is a record field access Gt_0#Name.Field, where Field is an atom, then Rep(Gt) = {record_field,ANNO,Rep(Gt_0),Name,Rep(Field)}.

If Gt is a record field index #Name.Field, where Field is an atom, then Rep(Gt) = {record_index,ANNO,Name,Rep(Field)}.

If Gt is a tuple skeleton {Gt_1, ..., Gt_k}, then Rep(Gt) = {tuple,ANNO,[Rep(Gt_1), ..., Rep(Gt_k)]}.

If Gt is a variable pattern V, then Rep(Gt) = {var,ANNO,A}, where A is an atom with a printname consisting of the same characters as V.

Notice that every guard test has the same source form as some expression, and is represented in the same way as the corresponding expression.

8.7  Types
If T is an annotated type A :: T_0, where A is a variable, then Rep(T) = {ann_type,ANNO,[Rep(A),Rep(T_0)]}.

If T is an atom, a character, or an integer literal L, then Rep(T) = Rep(L).

If T is a bitstring type <<_:M,_:_*N>>, where M and N are singleton integer types, then Rep(T) = {type,ANNO,binary,[Rep(M),Rep(N)]}.

If T is the empty list type [], then Rep(T) = {type,ANNO,nil,[]}, that is, the empty list type [] cannot be distinguished from the predefined type nil().

If T is a fun type fun(), then Rep(T) = {type,ANNO,'fun',[]}.

If T is a fun type fun((...) -> T_0), then Rep(T) = {type,ANNO,'fun',[{type,ANNO,any},Rep(T_0)]}.

If T is a fun type fun(Ft), where Ft is a function type, then Rep(T) = Rep(Ft). For Rep(Ft), see below.

If T is an integer range type L .. H, where L and H are singleton integer types, then Rep(T) = {type,ANNO,range,[Rep(L),Rep(H)]}.

If T is a map type map(), then Rep(T) = {type,ANNO,map,any}.

If T is a map type #{A_1, ..., A_k}, where each A_i is an association type, then Rep(T) = {type,ANNO,map,[Rep(A_1), ..., Rep(A_k)]}. For Rep(A), see below.

If T is an operator type T_1 Op T_2, where Op is a binary operator (this is an occurrence of an expression that can be evaluated to an integer at compile time), then Rep(T) = {op,ANNO,Op,Rep(T_1),Rep(T_2)}.

If T is an operator type Op T_0, where Op is a unary operator (this is an occurrence of an expression that can be evaluated to an integer at compile time), then Rep(T) = {op,ANNO,Op,Rep(T_0)}.

If T is ( T_0 ), then Rep(T) = Rep(T_0), that is, parenthesized types cannot be distinguished from their bodies.

If T is a predefined (or built-in) type N(T_1, ..., T_k), then Rep(T) = {type,ANNO,N,[Rep(T_1), ..., Rep(T_k)]}.

If T is a record type #Name{F_1, ..., F_k}, where each F_i is a record field type, then Rep(T) = {type,ANNO,record,[Rep(Name),Rep(F_1), ..., Rep(F_k)]}. For Rep(F), see below.

If T is a remote type M:N(T_1, ..., T_k), then Rep(T) = {remote_type,ANNO,[Rep(M),Rep(N),[Rep(T_1), ..., Rep(T_k)]]}.

If T is a tuple type tuple(), then Rep(T) = {type,ANNO,tuple,any}.

If T is a tuple type {T_1, ..., T_k}, then Rep(T) = {type,ANNO,tuple,[Rep(T_1), ..., Rep(T_k)]}.

If T is a type union T_1 | ... | T_k, then Rep(T) = {type,ANNO,union,[Rep(T_1), ..., Rep(T_k)]}.

If T is a type variable V, then Rep(T) = {var,ANNO,A}, where A is an atom with a printname consisting of the same characters as V. A type variable is any variable except underscore (_).

If T is a user-defined type N(T_1, ..., T_k), then Rep(T) = {user_type,ANNO,N,[Rep(T_1), ..., Rep(T_k)]}.

Function Types
A function type Ft is one of the following:

If Ft is a constrained function type Ft_1 when Fc, where Ft_1 is a function type and Fc is a function constraint, then Rep(T) = {type,ANNO,bounded_fun,[Rep(Ft_1),Rep(Fc)]}. For Rep(Fc), see below.

If Ft is a function type (T_1, ..., T_n) -> T_0, where each T_i is a type, then Rep(Ft) = {type,ANNO,'fun',[{type,ANNO,product,[Rep(T_1), ..., Rep(T_n)]},Rep(T_0)]}.

Function Constraints
A function constraint Fc is a non-empty sequence of constraints C_1, ..., C_k, and Rep(Fc) = [Rep(C_1), ..., Rep(C_k)].

If C is a constraint V :: T, where V is a type variable and T is a type, then Rep(C) = {type,ANNO,constraint,[{atom,ANNO,is_subtype},[Rep(V),Rep(T)]]}.
Association Types
If A is an association type K => V, where K and V are types, then Rep(A) = {type,ANNO,map_field_assoc,[Rep(K),Rep(V)]}.

If A is an association type K := V, where K and V are types, then Rep(A) = {type,ANNO,map_field_exact,[Rep(K),Rep(V)]}.

Record Field Types
If F is a record field type Name :: Type, where Type is a type, then Rep(F) = {type,ANNO,field_type,[Rep(Name),Rep(Type)]}.