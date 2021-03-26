-module(portogui).
-export([start/1, stop/0, loop/1, init/1, parse_data/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% port only wait data from and send to gui								%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(ExtPrg) ->
    spawn(?MODULE, init, [ExtPrg]).

stop() ->
    complex ! stop.

init(ExtPrg) ->
    register(complex, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn, ExtPrg}, [{packet, 2}]),
    erlout:start(),
    loop(Port).

loop(Port) ->
    receive
		{Port, {data, Data}} ->
			case parse_data(string:split(Data, "|", all)) of 
				{reply, Reply} ->
					Port ! {self(), Reply};
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

		{'EXIT', Port, _Reason} ->
			exit(port_terminated)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% analyse received data from gui										%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_data(["build diagramm"]) ->
	erlout:finite();
	
parse_data([Term | []]) ->
	io:format("~p\n", [Term]),
	{reply, "pong"};

parse_data([set_dir | Dir]) ->
	erlout:set_file(Dir);

parse_data([shade_modules | Modules]) ->
	erlout:shade_modules(Modules);
	
parse_data([shade_functions | Funs]) ->
	erlout:shade_functions(Funs);

parse_data([analyse_modules | Modules]) ->
	Modules.