-module(analyser).

-export([handle_request/1]).

handle_request("make_diagramm") ->
	erlout:finite();

handle_request(Request) ->
	[Head | Tail] = string:split(Request, "|"),
	handle_request(Head, Tail).

handle_request(ignore_modules, Modules) ->
	erlout:shade_modules(Modules);

handle_request(ignore_functions, Funs) ->
	erlout:shade_functions(Funs);

handle_request(output_path, Path) ->
	erlout:set_file(Path);

handle_request(get_functions, File) ->
	{reply, etoplan:get_functions(File)}.