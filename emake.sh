#!/usr/bin/env escript

main(Args) ->
	case filelib:wildcard(".", "erltoplan_GUI") of
		[] ->
			console_conf:run(Args);
		_ ->
		    portogui:init("./erltoplan_GUI")
	end.
