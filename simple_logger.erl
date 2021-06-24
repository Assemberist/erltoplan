-module(simple_logger).

-export([log/3, log/2]).

-export([
    war_1_not_reg_start_gen_server/1,
    war_2_gen_server_mod_arg_is_not_atom/1,
    war_3_gen_server_name_is_not_stable/1,
    war_4__gen_server_init_not_found/1,
    war_5_gen_server_cast_uncnown_server/1
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
    log("Warning #3: gen-server started with strange name~n~p~n~n", [Text]).

war_4__gen_server_init_not_found(Text) ->
    log("Warning #4: function init not found for gen_server module start~n~p~n~n", [Text]).

war_5_gen_server_call_uncnown_server(Text) ->
    log("Warning #5: Programm cannot define called module ~n~p~n~n", [Text]).
