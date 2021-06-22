-module(erlout).
-behavior(gen_server).

-define(server, {global, ?MODULE}).

-export([init/1, handle_call/3, handle_cast/2]).
-export([start/0, set/2, put/2, get/1, finite/0, reset/0]).

-define(default_state,
    #{
        file => "undefined.txt",    % string(),
        links => [],                % [{{atom(), atom()}, {atom(), atom()}}],
        shaded_modules => [],       % [atom()],
        shaded_functions => [],     % [atom()],

        analysed_files => [],       % [string()]
		gs_links => [],
		gs_servers => [],			% [{atom(), term()}]
		gs_ready => [],				% [{{atom(), atom()}, {atom(), atom()}}],

		trash => [] 				% [term()]
    }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% interface funs														%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start() ->
	gen_server:start_link(?server, ?MODULE, ?default_state, []).

set(Field, Value) ->
    gen_server:cast(?server, {set, {Field, Value}}).

-spec put(atom(), [term()]) -> ok.
	put(Field, Values) ->
    gen_server:cast(?server, {put, {Field, Values}}).

get(Field) ->
    gen_server:call(?server, {get, Field}).

reset() ->
	gen_server:cast(?server, reset).

finite() ->
	gen_server:call(?server, finite).

init(State) -> {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server part														%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_cast({set, {Field, Value}}, State) ->
    {noreply, State#{Field := Value}};

handle_cast({put, {Field, Values}}, State) ->
    OldValues = maps:get(Field, State),
    {noreply, State#{Field := Values ++ OldValues}};

handle_cast(reset, _) ->
    {noreply, ?default_state}.

handle_call({get, Field}, _, State) ->
    {reply, maps:get(Field, State), State};

handle_call(finite, _, State) ->
    File = maps:get(file, State),
    %% init UML
    file:write_file(File, "@startuml\n\n", [write]),

	%% split on calls and defenitions
	{Links, SingleLinks} = lists:partition(
		fun	({{_, _}, {_, _}}) -> true; (_) -> false end,
		maps:get(links, State)),

    %% remove duplicates
    ULinks = lists:usort(Links),

    %% remove shaded modules and functions
    UALinks = remove_shaded(ULinks, maps:get(shaded_modules, State), maps:get(shaded_functions, State)),

    %% get all funs and modules
    Modules = sort_calls(UALinks, SingleLinks),

    %% write all nodes
    maps:map(fun(Module, Value) -> put_node(File, Module, Value) end, Modules),

    %% write all links
    lists:map(fun(Value) -> put_link(File, Value) end, UALinks),

	%% write gen_server calls
	lists:map(fun(Value) -> put_gs_links(File, Value) end, maps:get(gs_ready, State)),

    %% end of UML
    file:write_file(File, "\n@enduml", [append]),
    {reply, ok, ?default_state}.

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

sort_calls(Calls, SingleLinks) ->
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
		lists:merge(lists:map(fun erlang:tuple_to_list/1, Calls)) ++ SingleLinks
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

put_gs_links(File, {{Mod1, Fun1}, {Mod2, Fun2}, Legend}) ->
	lists:map(fun(Value) -> file:write_file(File, Value, [append]) end,
		["[", atom_to_list(Mod1), ":", atom_to_list(Fun1), "] ..> [", 
			atom_to_list(Mod2), ":", atom_to_list(Fun2), "] : ", atom_to_list(Legend), "\n"]).