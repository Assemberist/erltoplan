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
            case binary_to_term(list_to_binary(Data)) of
                {query, Query} ->
                    Port ! answer_guery(Query);
                
                {config, Config} ->
                    Modules = set_config(Config),
                    lists:map(fun erltoplan:parse/1, Modules);

                %% kick out this shit
                %% but later
                {look_term, Term} ->
                    io:format("~p\n", [Term])

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

answer_guery({parse_for_functions, File}) ->
    {ok, Src} = epp:parse_file(File, []),
    Funs = [Fun#function.name || Fun <- Src, is_record(Fun, function)],
    lists:foldl(fun(Val, Acc) -> Acc ++ "+" ++ atom_to_list(Val) end, [], Funs);

answer_guery(_Query) ->
    ok.

%% parsing of received config
set_config([{analyse, Modules} | []]) ->
    Modules;

set_config([Head | Tail]) ->
    NewList = case Head of
        {output, Name} ->
            erlout:set_file(Name),
            Tail;

        {shade_modules, Modules} ->
            erlout:shade_modules(Modules),
            Tail;

        {shade_funs, Funs} ->
            erlout:shade_modules(Funs),
            Tail;

        {analyse, _} ->
            Tail ++ [Head]

    end,
    set_config(NewList).