-module(simple_logger).

-export([log/3, log/2]).

log(Format, Info) ->
    {ok, IO} = file:open("erltoplan.log", [append]),
    io:fwrite(IO, Format, Info),
    file:close(IO).

log(File, Format, Info) ->
    {ok, IO} = file:open(File, [append]),
    io:fwrite(IO, Format, Info),
    file:close(IO).