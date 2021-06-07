-module(console_conf).

-export([run/1, arg_handle/2]).

run(Args) ->
	erlout:start(),
	%try 
		lists:foldl(fun ?MODULE:arg_handle/2, files, Args),
		gs_parser:gs_parse(),
		io:format("~p~n", [erlout:get(gs_links)]),
		erlout:finite().
	%catch _:_:_ ->
	%	ok
	%end.

arg_handle("-h", _) -> 
    put_help(),
	exit(ok);

arg_handle("-i", _) ->
	ignore;

arg_handle("--no-std", State) ->
	erlout:put(shaded_modules, [erlang]),
	State;

arg_handle("-o", State) ->
    {name, State};

arg_handle(Arg, {name, State}) ->
    erlout:set(file, Arg),
    State;

arg_handle(Arg, files) ->
	parser:links(Arg),
	files;

arg_handle(Arg, ignore) ->
	case string:split(Arg, ":") of
		[Module | []] -> 
			erlout:put(shaded_modules, [list_to_atom(Module)]);
		[Module, Function] ->
			erlout:put(shaded_functions, [{list_to_atom(Module), list_to_atom(Function)}]);
		_ ->
			exit(fuck)
	end.

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
