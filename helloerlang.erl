-module(helloErlang).
-export([start/0]).

start () ->
	spawn(fun () -> 
		loop() 
		end
	).

loop () ->
	receive
		hello ->
			io:format("Hello erlang!\n"),
			loop();
		goodbye ->
			ok
	end.