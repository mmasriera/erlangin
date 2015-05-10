-module(mutysplit).
-export([start/3, stop/0]).

% We use the name of the module (i.e. lock3) as a parameter to the start procedure. We also provide the average time (in milliseconds) the worker is going to sleep before trying to get the lock (Sleep) and work with the lock taken (Work).

start(Lock,Sleep, Work) ->

    register(l3, apply(Lock, start, [3])),
    register(l4, apply(Lock, start, [4])),

    
    timer:sleep(2000),
    
    l3 ! {peers, [{l1,'laptop@192.168.0.101'}, {l2,'laptop@192.168.0.101'}, l4]},
    l4 ! {peers, [{l1,'laptop@192.168.0.101'}, {l2,'laptop@192.168.0.101'}, l3]},
   
    register(w3, worker:start("Paul", l3, Sleep, Work)),
    register(w4, worker:start("George", l4, Sleep, Work)),
    
    ok.

stop() ->
    w3 ! stop,
    w4 ! stop.
