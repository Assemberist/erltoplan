-module(console_conf).

-export([run/1, arg_handle/2]).

run(Args) ->
	erlout:start(),
	lists:foldl(fun ?MODULE:arg_handle/2, files, Args),
	erlout:finite().

arg_handle("-h", _) -> 
    put_help(),
	exit(ok);

arg_handle("-i", _) ->
	ignore;

arg_handle("--no-std", State) ->
	erlout:shade_modules([erlang]),
	State;

arg_handle("-o", State) ->
    {name, State};

arg_handle(Arg, {name, State}) ->
    erlout:set_file(Arg),
    State;

arg_handle(Arg, files) ->
	parser:links(Arg),
	files;

arg_handle(Arg, ignore) ->
	case string:split(Arg, ":") of
		[Module | []] -> 
			erlout:shade_modules([list_to_atom(Module)]);
		[Module, Function] ->
			erlout:shade_functions([{list_to_atom(Module), list_to_atom(Function)}]);
		_ ->
			exit(fuck)
	end.

put_help() ->
    os:cmd("cat doc/help.txt").