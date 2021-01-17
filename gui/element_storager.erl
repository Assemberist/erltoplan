-module(element_storager).
-behaviour(gen_server).

-export([start/0, push/2, get/1]).
-export([init/1, handle_call/3, handle_cast/2]).

start() ->
    gen_server:start({global, ?MODULE}, ?MODULE, [[]], []).

init(_) ->
    {ok, #{}}.

push(Id, Element) ->
    gen_server:cast({global, ?MODULE}, {push, {Id, Element}}).

get(Id) ->
    gen_server:call({global, ?MODULE}, {get, Id}).



handle_call({get, Id}, _, State) ->
    {reply, maps:get(Id, State), State}.

handle_cast({push, {Id, Element}}, State) ->
    {noreply, State#{Id => Element}}.