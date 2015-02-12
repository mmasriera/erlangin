
-module(hello).	%file name
-export([helloWorld/0]).

	% compile	> c(hello).
	% exec		> hello:helloWorld().

helloWorld() -> io:fwrite("hello erlang!\n").

