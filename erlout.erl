-module(erlout).
-behavior(gen_server).

-export([start_wrapper/0, init_file/1, write_export/1, write_link/2, finite/0]).
-export([init/1, handle_call/3, handle_cast/2]).

-record(wrapper_state, {file}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Interfaces																%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_wrapper() -> 
	gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

init_file(File) -> 
	gen_server:cast(?MODULE, {init_file, File}).
write_export(Exports) -> 
	gen_server:cast(?MODULE, {write_export, Exports}).
write_link(Caller, Called) -> 
	gen_server:cast(?MODULE, {write_link, {Caller, Called}}).
finite() -> 
	gen_server:cast(?MODULE, finite).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Interfaces																%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(_Arg) ->
	{ok, #wrapper_state{}}.
	
handle_cast({init_file, File}, State) ->
	file:write_file(File, "@startuml\n", [write]),
	{noreply, State#wrapper_state{file = File}};

handle_cast({write_export, Exports}, State) ->
	file:write_file(State#wrapper_state.file, "node \"exports\" {\n", [append]),
	lists:map(
		fun(Element) -> 
			lists:map(fun(Value) -> file:write_file(State#wrapper_state.file, Value, [append]) end,
				["\t[", Element, "]\n"])
		end, 
		Exports),
	file:write_file(State#wrapper_state.file, "}\n", [append]),
	{noreply, State};

handle_cast({write_link, {Caller, Called}}, State) ->
	lists:map(fun(Value) -> file:write_file(State#wrapper_state.file, Value, [append]) end,
		["[", Caller, "] --> [", Called, "]\n"]),
	{noreply, State};

handle_cast(finite, State) ->
	file:write_file(State#wrapper_state.file, "@enduml", [append]),
	{noreply, #wrapper_state{}}.

handle_call(_, _, State) ->
	{reply, {error, no_calls}, State}.