-module(simple_logger).

-export([log/3, log/2]).

-export([
    war_1_not_reg_start_gen_server/1,
    war_2_gen_server_mod_arg_is_not_atom/1,
    war_3_gen_server_name_is_not_stable/1
]).

log(Format, Info) ->
    {ok, IO} = file:open("erltoplan.log", [append]),
    io:fwrite(IO, Format, Info),
    file:close(IO).

log(File, Format, Info) ->
    {ok, IO} = file:open(File, [append]),
    io:fwrite(IO, Format, Info),
    file:close(IO).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% warnings     														%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% programm can not define who can use gen-server started
%% without registration 
war_1_not_reg_start_gen_server(Text) ->
    log("Warning #1: gen-server started by start function with arity 3~n~p~n~n", [Text]).

%% programm can define what module will be started 
%% only if it is transisted directly
war_2_gen_server_mod_arg_is_not_atom(Text) ->
    log("Warning #2: gen-server started but arg \"module\" is no atom~n~p~n~n", [Text]).

war_3_gen_server_name_is_not_stable(Text) ->
    log("Warning #2: gen-server started with strange name~n~p~n~n", [Text]).
