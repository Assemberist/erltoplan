-record(function, {anno, name, arity, enrtyes}). %% {function,ANNO,Name,Arity,[Rep(Fc_1), ...,Rep(Fc_k)]}.

-record(attribute, {anno, type, value}).

-define(attr_export, #attribute{type = export}). %% {attribute,ANNO,export,[{Fun_1,A_1}, ..., {Fun_k,A_k}]}.
-define(attr_import, #attribute{type = import}). %% {attribute,ANNO,import,{Mod,[{Fun_1,A_1}, ..., {Fun_k,A_k}]}}.
-define(attr_module, #attribute{type = module}). %% {attribute,ANNO,module,Mod}.
-define(attr_file, #attribute{type = file}). %% {attribute,ANNO,file,{File,Line}}.
-define(attr_spec, #attribute{type = spec}). %% {attribute,ANNO,spec,{{Mod,Name,Arity},[Rep(Ft_1), ..., Rep(Ft_k)]}}.
-define(attr_record, #attribute{type = record}). %% {attribute,ANNO,record,{Name,[Rep(V_1), ..., Rep(V_k)]}}.
%% {attribute,ANNO,Spec,{{Name,Arity},[Rep(Ft_1), ..., Rep(Ft_k)]}}.
%% {attribute,ANNO,Type,{Name,Rep(T),[Rep(V_1), ..., Rep(V_k)]}}.
%% {attribute,ANNO,A,T}.
-define(attr_spec, #attribute{type = spec}). 

