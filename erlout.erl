-module(erlout).
-behavior(gen_server).

-define(server, {global, ?MODULE}).

-export([init/1, handle_call/3, handle_cast/2]).
-export([start/0, set_file/1, write_exports/1, write_link/2, write_far_link/3, finite/0, 
		shade_modules/1, shade_functions/1
	]).
-export([fart_olink/2]).

-record(state, {
	file 					:: string(), 
	external = #{} 			:: #{atom() => [atom()]},
	links = [] 				:: [{atom(), atom()}],
	shaded_modules = [] 	:: [atom()],
	shaded_functions = [] 	:: [atom()]
}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% interface funs														%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start() ->
	gen_server:start_link(?server, ?MODULE, [], []).

set_file(File) ->
	gen_server:cast(?server, {set_file, File}).

write_exports(Exports) ->
	[gen_server:cast(?server, {add_fart, exports, Export}) || Export <- Exports].

write_link(Caller, Called) ->
	gen_server:cast(?server, {add_link, Caller, Called}).

write_far_link(Caller, Module, Called) ->
	Converted = fart_olink(Module, Called),
	case gen_server:call(?server, {add_fart, Module, Converted}) of
		ok -> write_link(Caller, Converted);
		_ -> ok
	end.

shade_modules(ModuleList) ->
	gen_server:cast(?server, {shade_modules, ModuleList}).

shade_functions(FunList) ->
	gen_server:cast(?server, {shade_functions, FunList}).

finite() ->
	gen_server:call(?server, finite).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server part														%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(_) -> {ok, #state{}}.

handle_cast({set_file, File}, State) ->
	{noreply, State#state{file = File}};

handle_cast({add_link, Caller, Called}, State = #state{links = Links}) ->
	NewLinks = 
		case lists:member(Caller, State#state.shaded_functions) of
			true ->
				Links;

			_ -> 
				case lists:member({Caller, Called}, Links) of
					true -> Links;
					_ -> [{Caller, Called} | Links]
				end
		end,

	{noreply, State#state{links = NewLinks}};

handle_cast({shade_modules, ModuleList}, State) ->
	{noreply, State#state{shaded_modules = ModuleList}};

handle_cast({shade_functions, FunList}, State) ->
	{noreply, State#state{shaded_functions = FunList}};

handle_cast(_, State) -> {noreply, State}.

handle_call({add_fart, Module, Fart}, _, State = #state{external = FarList}) ->
	{Changes, NewList} = 
		case lists:member(Module, State#state.shaded_modules) of
			true ->
				{nok, FarList};

			_ -> 
				case maps:is_key(Module, FarList) of
					true -> 
						#{Module := Value} = FarList,
						case lists:member(Fart, Value) of
							true -> {ok, FarList};
							false -> {ok, FarList#{Module => [Fart | Value]}}
						end;
					false ->
						{ok, FarList#{Module => [Fart]}}
				end
		end,
		
	{reply, Changes, State#state{external = NewList}};

handle_call(finite, _, #state{file = File, external = Ext, links = Links}) ->
	%% init UML
	file:write_file(File, "@startuml\n\n", [write]),

	%% write all nodes
	maps:map(fun(Module, Value) -> put_node(File, Module, Value) end, Ext),

	%% write all links
	lists:map(fun(Value) -> put_link(File, Value) end, Links),

	%% end of UML
	file:write_file(File, "\n@enduml", [append]),
	{reply, ok, #state{}};

handle_call(_, _, State) ->
	{reply, {error, you_pidor}, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  iternal functions													%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fart_olink(A, B) ->
	list_to_atom(atom_to_list(A) ++ ":" ++ atom_to_list(B)).

put_node(File, NodeName, Elementes) ->
	file:write_file(File, "node \"" ++ atom_to_list(NodeName) ++ "\" {\n", [append]),
	lists:map(
		fun(Element) -> 
			lists:map(fun(Value) -> file:write_file(File, Value, [append]) end,
				["\t[", atom_to_list(Element), "]\n"])
		end, 
		Elementes),
	file:write_file(File, "}\n\n", [append]).

put_link(File, {Caller, Called}) ->
	lists:map(fun(Value) -> file:write_file(File, Value, [append]) end,
		["[", atom_to_list(Caller), "] --> [", atom_to_list(Called), "]\n"]).
