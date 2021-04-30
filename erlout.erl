-module(erlout).
-behavior(gen_server).

-define(server, {global, ?MODULE}).

-export([init/1, handle_call/3, handle_cast/2]).
-export([start/0, set_file/1, write_links/1, finite/0,
		shade_modules/1, shade_functions/1, reset/0
	]).

-record(state, {
	file 						:: string(),
	links = [] 					:: [{{atom(), atom()}, {atom(), atom()}}],
	shaded_modules = [] 		:: [atom()],
	shaded_functions = [] 		:: [atom()]
}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% interface funs														%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start() ->
	gen_server:start_link(?server, ?MODULE, [], []).

set_file(File) ->
	gen_server:cast(?server, {set_file, File}).

write_links(Links) ->
	gen_server:cast(?server, {write_links, Links}).

shade_modules(ModuleList) ->
	gen_server:cast(?server, {shade_modules, ModuleList}).

shade_functions(FunList) ->
	gen_server:cast(?server, {shade_functions, FunList}).

reset() ->
	gen_server:cast(?server, reset).

finite() ->
	gen_server:call(?server, finite).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server part														%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(_) -> {ok, #state{}}.

handle_cast({set_file, File}, State) ->
	{noreply, State#state{file = File}};

handle_cast({shade_modules, ModuleList}, State) ->
	{noreply, State#state{shaded_modules = ModuleList}};

handle_cast({shade_functions, FunList}, State) ->
	{noreply, State#state{shaded_functions = FunList}};

handle_cast({write_links, Links}, State = #state{links = OldLinks}) ->
	{noreply, State#state{links = OldLinks ++ Links}};

handle_cast(reset, _) -> {noreply, #state{}};

handle_cast(_, State) -> {noreply, State}.

handle_call(finite, _, State = #state{file = File}) ->
	%% init UML
	file:write_file(File, "@startuml\n\n", [write]),

	%% remove duplicates
	ULinks = lists:usort(State#state.links),

	%% remove shaded modules and functions
	UALinks = remove_shaded(ULinks, State#state.shaded_modules, State#state.shaded_functions),

	%% get all funs and modules
	Modules = sort_calls(UALinks),

	%% write all nodes
	maps:map(fun(Module, Value) -> put_node(File, Module, Value) end, Modules),

	%% write all links
	lists:map(fun(Value) -> put_link(File, Value) end, UALinks),

	%% end of UML
	file:write_file(File, "\n@enduml", [append]),
	{reply, ok, #state{}};

handle_call(_, _, State) ->
	{reply, {error, you_pidor}, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  iternal functions													%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

put_node(File, NodeName, Elementes) ->
	file:write_file(File, "node \"" ++ atom_to_list(NodeName) ++ "\" {\n", [append]),
	lists:map(
		fun(Element) -> 
			lists:map(fun(Value) -> file:write_file(File, Value, [append]) end,
				["\t[", atom_to_list(NodeName), ":", atom_to_list(Element), "]\n"])
		end, 
		Elementes),
	file:write_file(File, "}\n\n", [append]).

put_link(File, {{Mod1, Fun1}, {Mod2, Fun2}}) ->
	lists:map(fun(Value) -> file:write_file(File, Value, [append]) end,
		["[", atom_to_list(Mod1), ":", atom_to_list(Fun1), "] --> [", 
			atom_to_list(Mod2), ":", atom_to_list(Fun2), "]\n"]).

sort_calls(Calls) ->
	lists:foldl(
		fun({Mod, Fun}, Map)->
			case maps:find(Mod, Map) of
				{ok, Val} ->
					case lists:member(Fun, Val) of
						true ->
							Map;
						_ ->
							Map#{Mod := [Fun | Val]}
					end;
				_ ->
					Map#{Mod => [Fun]}
			end
		end,
		#{},
		lists:merge(lists:map(fun erlang:tuple_to_list/1, Calls))
	).

remove_shaded(Links, Modules, FunList) ->
	RemMods = lists:filter(
		fun({{M1, _}, {M2, _}}) ->
			case lists:member(M1, Modules) or
				 lists:member(M2, Modules) of
				true -> false;
				_ -> true
			end
		end,
		Links),
	
	lists:filter(
		fun({Caller, Called}) ->
			not (lists:member(Caller, FunList) or lists:member(Called, FunList)) 
		end, 
		RemMods).
