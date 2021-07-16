-module(erlout).
-behavior(gen_server).

-include("termanus.hrl").

-define(server, {global, ?MODULE}).

-export([init/1, handle_call/3, handle_cast/2]).
-export([start/0, set/2, put/2, get/1, finite/0, trace/1, reset/0]).

-define(default_state,
    #{
        file => "undefined.txt",    % string(),
        links => [],                % [#link{} | farFunction()],
        shaded_modules => [],       % [atom()],
        shaded_functions => [],     % [atom()],

        analysed_files => [],       % [string()]
		gs_servers => [],			% [{atom(), term()}]
		gs_starts => [],			% [{{atom(), #function{}}, #call{}}],
		gs_calls => [],				% [{{atom(), #function{}}, #call{}}],
		gs_casts => [],				% [{{atom(), #function{}}, #call{}}],
		gs_ready => [],				% 

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
	gen_server:call(?server, finite, infinity).

trace(Target) ->
	gen_server:call(?server, {trace, Target}, infinity).

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

    %% remove duplicates
    ULinks = lists:usort(maps:get(links, State)),

    %% remove shaded modules and functions
    UALinks = remove_shaded(ULinks, maps:get(shaded_modules, State), maps:get(shaded_functions, State)),

	%% split on calls and defenitions
	{Links, SingleLinks} = lists:partition(
		fun	(#link{}) -> true; (_) -> false end,
		UALinks),

    %% get all funs and modules
    Modules = sort_calls(Links, SingleLinks),

    %% write all nodes
    maps:map(fun(Module, Value) -> put_node(File, Module, Value) end, Modules),

    %% write all links
    lists:map(fun(Value) -> put_link(File, Value) end, Links),

	%% write gen_server calls
	lists:map(fun(Value) -> put_link(File, Value) end, maps:get(gs_ready, State)),

    %% end of UML
    file:write_file(File, "\n@enduml", [append]),
    {reply, ok, ?default_state};

handle_call({trace, {Target, Mode}}, _, State) ->
    File = maps:get(file, State),
    %% init UML
    file:write_file(File, "@startuml\n\n", [write]),

    %% remove duplicates
    ULinks = lists:usort(maps:get(links, State)),

    %% remove shaded modules and functions
    UALinks = remove_shaded(ULinks, maps:get(shaded_modules, State), maps:get(shaded_functions, State)),

	%% split on calls and defenitions
	Links = lists:filter(
		fun	(#link{}) -> true; (_) -> false end,
		UALinks),

	BLinks = case Mode of
		up ->
			bind_links_up([], Links, Target, []);
		down ->
			bind_links_down([], Links, Target, []);
		up_down ->
			bind_links_up([], Links, Target, []) ++ bind_links_down([], Links, Target, [])
		end,

    %% get all funs and modules
    Modules = sort_calls(BLinks, []),

    %% write all nodes
    maps:map(fun(Module, Value) -> put_node(File, Module, Value) end, Modules),

    %% write all links
    lists:map(fun(Value) -> put_link(File, Value) end, BLinks),

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

put_link(File, {link, {Mod1, Fun1}, {Mod2, Fun2}}) ->
	lists:map(fun(Value) -> file:write_file(File, Value, [append]) end,
		["[", atom_to_list(Mod1), ":", atom_to_list(Fun1), "] --> [", 
			atom_to_list(Mod2), ":", atom_to_list(Fun2), "]\n"]);

put_link(File, {Legend, {Mod1, Fun1}, {Mod2, Fun2}}) ->
	lists:map(fun(Value) -> file:write_file(File, Value, [append]) end,
		["[", atom_to_list(Mod1), ":", atom_to_list(Fun1), "] ..> [", 
			atom_to_list(Mod2), ":", atom_to_list(Fun2), "] : ", atom_to_list(Legend), "\n"]).
		
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
		lists:merge([[Caller, Called] || {_, Caller, Called} <- Calls]) ++ SingleLinks
	).

remove_shaded(Links, Modules, FunList) ->
	RemMods = lists:filter(
		fun
			({_, {M1, _}, {M2, _}}) ->
				not (lists:member(M1, Modules) or lists:member(M2, Modules));
			({M1, _}) -> 
				not lists:member(M1, Modules)
		end,
		Links),
	
	lists:filter(
		fun
			({_, Caller, Called}) ->
				not (lists:member(Caller, FunList) or lists:member(Called, FunList));
			(Link) ->
				not (lists:member(Link, FunList))
		end, 
		RemMods).

bind_links_up(OldCallers, List, Called, Acc) ->
	{NewCallers, Other} = get_callers(Called, List),
	Callers = OldCallers ++ NewCallers,
	case Callers of 
		[] -> Acc;
		[#link{caller = Caller} | Tail] ->
			bind_links_up(Tail, Other, Caller, Acc ++ NewCallers)
	end.

get_callers(Called, List) ->
	lists:partition(fun(X) when X#link.called == Called -> true; (_) -> false end, List).

bind_links_down(OldCalled, List, Caller, Acc) ->
	{NewCalled, Other} = get_called(Caller, List),
	Callers = OldCalled ++ NewCalled,
	case Callers of 
		[] -> Acc;
		[#link{called = Called} | Tail] ->
			bind_links_down(Tail, Other, Called, Acc ++ NewCalled)
	end.

get_called(Caller, List) ->
	lists:partition(fun(X) when X#link.caller == Caller -> true; (_) -> false end, List).