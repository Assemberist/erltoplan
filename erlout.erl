-module(erlout).

-export([init/1, write_export/2, write_link/2, finite/1]).

init(File) -> file:write_file(File, "@startuml\n", [write]).

write_export(File, Exports) ->
	file:write_file(File, "node \"exports\" {\n", [append]),
	lists:map(
		fun(Element) -> 
			lists:map(fun(Value) -> file:write_file(File, Value, [append]) end,
				["\t[", atom_to_binary(Element), "]\n"])
		end, 
		Exports),
	file:write_file(File, "}\n", [append]).

write_link(File, {Caller, Called}) ->
	lists:map(fun(Value) -> file:write_file(File, Value, [append]) end,
		["[", Caller, "] --> [", Called, "]\n"]).

finite(File) -> file:write_file(File, "@enduml", [append]).
