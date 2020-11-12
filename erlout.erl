-module(erlout).
-behavior(gen_server).

-define(server, {global, ?MODULE}).

-export([init/1, handle_call/3, handle_cast/2]).
-export([start/0, set_file/1, write_export/1, write_link/2, write_far_link/3, finite/0]).
-export([fart_olink/2]).

-record(state, {
	file :: string(), 
	external = #{} :: #{atom() => [atom()]},
	links = [] :: [{atom(), atom()}]
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
	gen_server:cast(?server, {add_fart, Module, Converted}),
	write_link(Caller, Converted).

finite() ->
	gen_server:cast(?server, finite).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server part														%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(_) -> {ok, #state{}}.

handle_cast({set_file, File}, State) ->
	{noreply, State#state{file = File}};

handle_cast({add_link, Caller, Called}, State = #state{links = Links}) ->
	NewLinks = case lists:member({Caller, Called}, Links) of
		true -> Links;
		false -> [{Caller, Called} | Links]
	end,
	{noreply, State#state{links = NewLinks}};

handle_cast({add_fart, Module, Fart}, State = #state{external = FarList}) ->
	NewList = case maps:is_key(Module, FarList) of
		true -> 
			#{Module := Value} = FarList,
			case lists:member(Fart, Value) of
				true -> FarList;
				false -> FarList#{Module => [Fart | Value]}
			end;
		false ->
			FarList#{Module => [Fart]}
	end,
	{noreply, State#state{external = NewList}};


handle_cast(finite, #state{file = File, external = Ext, links = Links}) ->
	%% init UML
	file:write_file(File, "@startuml\n\n", [write]),

	%% write all nodes
	maps:map(fun(Module, Value) -> put_node(File, Module, Value) end, Ext),

	%% write all links
	lists:map(fun(Value) -> put_link(File, Value) end, Links),

	%% end of UML
	file:write_file(File, "@enduml", [append]),
	{noreply, #state{}}.

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
