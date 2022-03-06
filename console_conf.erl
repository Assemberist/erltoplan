-module(console_conf).

-include("termanus.hrl").

-export([run/1, arg_handle/2]).

run(Args) ->
	erlout:start(),
	lists:foldl(fun arg_handle/2, config_all, Args),
	
	LinksConf = merge_config(erlout:get(config_all), erlout:get(config_links)),
	erlout:set(config_links, LinksConf),
	lists:map(fun parser:links/1, LinksConf#config.files),
	
	GsLinksConf = merge_config(erlout:get(config_all), erlout:get(config_genServers)),
	erlout:set(config_genServers, GsLinksConf),
	gs_parser:gs_parse(GsLinksConf#config.files),
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
	erlout:set(Mode, NewIList),
	{Mode, ignore};

arg_handle("-t", _) ->
	trace;

arg_handle(Arg, trace) ->
	[Module, Function] = string:split(Arg, ":"),
	erlout:set(format, {{trace, up_down}, {list_to_atom(Module), list_to_atom(Function)}}),
	trace;

arg_handle("-tup", _) ->
	trace_up;

arg_handle(Arg, trace_up) ->
	[Module, Function] = string:split(Arg, ":"),
	erlout:set(format, {{trace, up}, {list_to_atom(Module), list_to_atom(Function)}}),
	trace_up;

arg_handle("-tdown", _) ->
	trace_down;

arg_handle(Arg, trace_down) ->
	[Module, Function] = string:split(Arg, ":"),
	erlout:set(format, {{trace,down}, {list_to_atom(Module), list_to_atom(Function)}}),
	trace_down;

arg_handle("-s", State) ->
	erlout:set(format, simple),
	State;

arg_handle(Arg, Mode) ->
	Config = erlout:get(Mode),
	erlout:set(Mode, Config#config{files = [Arg | Config#config.files]}),
	Mode.

put_help() ->
    io:format(
"
emake.sh - script for building diagramms for erlang modules.

syntax:
    emake.sh [Mode [<Modules>]] [-i <Ignore list>] [Flags] [Output]

    Note: -h option shall end execution of script after show help.

Mode - type of analysis:

    -l          Calls of functions
    -g          gen_server calls
    -a          All modes above (selected by default)

<Modules> - List of files that should be analysed.
    Template: erlout.erl ...

<Ignore list> - List of modules and functions that should be hidden.
    Argument without ':' will be interpreted as name of module.
    Argument that matched 'module:function' will be interpreted as function.
    Argument that contain 2 and more ':' will lead to the end with error. 

    Template: erlout parser:links ...
	
    Note! Ignore list will be filled for last selected mode.

Flags:

    -i          Begin of ignore list. All next elemets except flags will be 
                interpreted as part of ignore list until Mode will be not changed.
    -o          Name output file. (default name of output file - undefined.txt)
    -h          show this text

Output - way to present data:
	
	-s                          Show all calls (selected by default)
    -tup <module:function>      Show all calls that can to call module:function
    -tdown <module:function>    Show all calls that triggered when module:function
    -t <module:function>        Show all calls that triggered when module:function
	                            called and who can to call  it

Usage:
    emake.sh -h
    emake.sh -l parser.erl -i lists 
    emake.sh erlout.erl -i lists -l parser.erl -i epp:parse_file -o output.txt
    emake.sh parser.erl -o output.txt -t parser:links


").

merge_config(Conf1, Conf2) ->
	#config{
		files = lists:usort(Conf1#config.files ++ Conf2#config.files),
		filters = #filter{
			modules = lists:usort(Conf1#config.filters#filter.modules ++ Conf2#config.filters#filter.modules),
			funs = lists:usort(Conf1#config.filters#filter.funs ++ Conf2#config.filters#filter.funs)
		}
	}.