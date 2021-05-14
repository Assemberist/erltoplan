-module(portogui).
-export([start/1, stop/0, loop/1, init/1, parse_data/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% port only wait data from and send to gui								%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(ExtPrg) ->
    spawn(?MODULE, init, [ExtPrg]).

stop() ->
    portogui ! stop.

init(ExtPrg) ->
    register(portogui, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn, ExtPrg}, [{packet, 2}]),
    erlout:start(),
    loop(Port).

loop(Port) ->
    receive
		{Port, {data, Data}} ->
			case parse_data(string:split(Data, "|", all)) of 
				{reply, Reply} ->
					port_command(Port,  Reply);
				_ -> 
					ok
			end, 			
			loop(Port);

		stop ->
			Port ! {self(), close},
			receive
				{Port, closed} ->
					exit(normal)
			end;

		{'EXIT', Port, Reason} ->
			exit(Reason)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% analyse received data from gui										%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_data(["build diagramm"]) ->
	erlout:finite();

parse_data(["set_dir" | Dir]) ->
	erlout:set_file(Dir);

parse_data(["shade_modules" | Modules]) ->
	erlout:shade_modules([list_to_atom(Mod) || Mod <- Modules]);
	
parse_data(["shade_functions" | Funs]) ->
	erlout:shade_functions([{list_to_atom(Mod), list_to_atom(Name)} || [Mod, Name] <- [lists:split(Fun, ":") || Fun <- Funs]]);
	
parse_data(["get_functions", Module]) ->
	{reply, string:join([atom_to_list(Fun) || Fun <- parser:get_functions(Module)], "|")};

parse_data(["analyse_modules" | Modules]) ->
	lists:foreach(fun parser:links/1, Modules);

parse_data([reset_config]) ->
	erlout:reset();

parse_data(_) ->
	ok.
