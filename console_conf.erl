-module(console_conf).

-include("termanus.hrl").

-export([run/1, arg_handle/2]).

run(Args) ->
	erlout:start(),
	lists:foldl(fun arg_handle/2, config_all, Args),
	erlout:finite().

arg_handle("-h", _) -> 
    put_help(),
	exit(ok);

arg_handle("-i", State = {_, ignore}) ->
	State;

arg_handle("-i", Mode) ->
	{Mode, ignore};

arg_handle("-o", State) ->
    {name, State};

arg_handle(Arg, {name, State}) ->
    erlout:set(file, Arg),
    State;

arg_handle("-l", _) ->
	config_links;

arg_handle("-g", _) ->
	config_genServers;

arg_handle("-a", _) ->
	config_all;

arg_handle(Arg, {Mode, ignore}) ->
	%% fuckung record definition!!! 
	%% Record field cannot be variable. Next code not work!!!
	%% IList = Config#state.Mode#config.filters,
	Config = erlout:get(Mode),
	NewIList = case string:split(Arg, ":") of
		[Module | []] -> 
			Config#config{filters = #filter{modules = [list_to_atom(Module) | Config#config.filters#filter.modules]}};
		[Module, Function] ->
			Config#config{filters = #filter{funs = [{list_to_atom(Module), list_to_atom(Function)} | Config#config.filters#filter.funs]}};
		_ ->
			exit(fuck)
	end,
	erlout:set(Mode, NewIList);

arg_handle("-t", _) ->
	trace;

arg_handle(Arg, trace) ->
	[Module, Function] = string:split(Arg, ":"),
	erlout:set(format, {trace, {Module, Function}}),
	trace;

arg_handle("-tup", _) ->
	trace_up;

arg_handle(Arg, trace_up) ->
	[Module, Function] = string:split(Arg, ":"),
	erlout:set(format, {trace_up, {Module, Function}}),
	trace_up;

arg_handle("-tdown", _) ->
	trace_down;

arg_handle(Arg, trace_down) ->
	[Module, Function] = string:split(Arg, ":"),
	erlout:set(format, {trace_down, {Module, Function}}),
	trace_down;

arg_handle(Arg, Mode) ->
	Config = erlout:get(Mode),
	erlout:set(Mode, Config#config{files = [Arg | Config#config.files]}),
	Mode.

put_help() ->
    io:format(
"
emake.sh - script for building diagramms for erlang modules.

syntax:
    emake.sh <Modules> [-i <Ignore list>] [flags]
    emake.sh -h

    Note: -h option shall end execution of script after show help.

<Modules> - List of files that should be analysed.
    Template: erlout.erl ...

<Ignore list> - List of modules and functions that should be hidden. 
    Argument without ':' will be interpreted as name of module.
    Argument that matched 'module:function' will be interpreted as function.
    Argument that contain 2 and more ':' will lead to the end with error. 

    Template: erlout parser:links ...

flags:

    -i          Begin of ignore list. All next elemets except flags will be 
                interpreted as part of ignore list.
    --no-std    Add std functions to the ignore list.
    -o          Name output file.
    -h          show this text
").
