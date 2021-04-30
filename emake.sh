#!/usr/bin/env escript

main(Args) ->
	case filelib:wildcard(".", "erltoplan_GUI") of
		[] ->
			console_conf(Args);
		_ ->
		    portogui:init("./erltoplan_GUI")
	end.

console_conf(Args) ->
	erlout:start(),
	lists:foldl(fun arg_handle/2, files, Args),
	erlout:finite().

arg_handle("-h", _) -> 
	put_help(),
	exit(ok);

arg_handle("-i", _) ->
	ignore;

arg_handle("--no-std", State) ->
	erlout:shade_modules("erlang"),
	State;

arg_handle(Arg, files) ->
	parser:links(Arg),
	files;

arg_handle(Arg, ignore) ->
	case string:split(Arg, ":") ->
		[Module | []] -> 
			erlout:shade_modules(list_to_atom(Module));
		[Module, Function] ->
			erlout:shade_functions({list_to_atom(Module), list_to_atom(Finction)});
		_ ->
			exit(fuck)
	end.
