-module(portogui).

-include("/home/sanya/sources/erlang/erltoplan/termanus.hrl").

-export([start/1, stop/0, loop/1, init/1]).

start(ExtPrg) ->
	store:start(),
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
			Term = parse_data(Data),
			case analyser:handle_request(Term) of 
				{reply, Reply} ->
					Port ! {self(), Reply},
				_ -> 
					ok;
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

parse_data(Data) ->
	Data.