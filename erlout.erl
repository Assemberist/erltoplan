-module(erlout).
-behavior(gen_server).

-include("termanus.hrl").

-define(server, {global, ?MODULE}).

-export([init/1, handle_call/3, handle_cast/2]).
-export([start/0, set/2, put/2, get/1, finite/0, reset/0]).

-define(default_state,
    #{
		config_all => #config{},
		config_links => #config{},
		config_genServers => #config{},
		format => simple,			%atom() | {atom(), {atom(), atom()}},

        file => "undefined.txt",    % string(),
        links => [],                % [#link{} | farFunction()],

        analysed_files => [],       % [string()]
		gs_servers => [],			% [{atom(), term()}]
		gs_starts => [],			% [{{atom(), #function{}}, #call{}}],
		gs_calls => [],				% [{{atom(), #function{}}, #call{}}],
		gs_casts => [],				% [{{atom(), #function{}}, #call{}}],
		gs_ready => [],				% [{call | cast | start, farFunction(), farFunction()}]

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

	%% sort and remove shaded links
	FilteredGsLinks = filter_links(maps:get(gs_ready, State), (maps:get(config_genServers, State))#config.filters),
	FilteredLinks = filter_links(maps:get(links, State), (maps:get(config_links, State))#config.filters),

	%% if we analysing gs then gen_server links duplicating gs_links.
	%% it is patch and should be removed in major versiosns.
	%% Filtered2Links = case FilteredGsLinks of 
	%%	[] -> FilteredLinks;
	%%	_ -> remove_shaded(FilteredLinks, [gen_server], [])
	%% end,
	%% it should be removed if file analysed by both modules.

	%% split on calls and defenitions
	{Links, SingleLinks} = lists:partition(
		fun	(#link{}) -> true; (_) -> false end,
		FilteredLinks),
	
	AllLinks = Links ++ FilteredGsLinks,

	%% get all funs and modules
    Modules = case maps:get(format, State) of
		{{trace, Mode}, Target} ->
			BLinks = case Mode of
				up ->
					bind_links_up([], AllLinks, Target, []);
				down ->
					bind_links_down([], AllLinks, Target, []);
				up_down ->
					bind_links_up([], AllLinks, Target, []) ++ bind_links_down([], AllLinks, Target, [])
				end,
			sort_calls(BLinks, []);

		simple ->
			BLinks = AllLinks,
			sort_calls(AllLinks, SingleLinks)
	end,

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
		[{_, Caller, _} | Tail] ->
			bind_links_up(Tail, Other, Caller, Acc ++ NewCallers)
	end.

get_callers(Called, List) ->
	lists:partition(fun({_, _, X}) when X == Called -> true; (_) -> false end, List).

bind_links_down(OldCalled, List, Caller, Acc) ->
	{NewCalled, Other} = get_called(Caller, List),
	Callers = OldCalled ++ NewCalled,
	case Callers of 
		[] -> Acc;
		[{_, _, Called} | Tail] ->
			bind_links_down(Tail, Other, Called, Acc ++ NewCalled)
	end.

get_called(Caller, List) ->
	lists:partition(fun({_, X, _}) when X == Caller -> true; (_) -> false end, List).

filter_links(Links, Filter) ->
	remove_shaded(lists:usort(Links), Filter#filter.modules, Filter#filter.funs).