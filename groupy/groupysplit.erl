-module(groupysplit).
-export([start/2, stop/0]).

% We use the name of the module (i.e. gms3) as the parameter Module to the start procedure. Sleep stands for up to how many milliseconds the workers should wait until the next message is sent.

start(Module, Sleep) ->
    spawn('p1@127.0.0.1', fun() -> group_leader(whereis(user), self()), 
      register(a, worker:start("P1", Module, Sleep))
        end),
    
    spawn('p2@127.0.0.1', fun() -> group_leader(whereis(user), self()), 
        register(b, worker:start("P2", Module, {a,'p1@127.0.0.1'}, Sleep)) 
        end),
    spawn('p3@127.0.0.1', fun() -> group_leader(whereis(user), self()), 
        register(c, worker:start("P3", Module, {a,'p1@127.0.0.1'}, Sleep)) 
        end),
    spawn('p4@127.0.0.1', fun() -> group_leader(whereis(user), self()), 
        register(d, worker:start("P4", Module, {a,'p1@127.0.0.1'}, Sleep)) 
        end),
    spawn('p5@127.0.0.1', fun() -> group_leader(whereis(user), self()), 
        register(e, worker:start("P5", Module, {a,'p1@127.0.0.1'}, Sleep)) 
        end).

stop() ->
    stop({a,'p1@127.0.0.1'}),
    stop({b,'p2@127.0.0.1'}),
    stop({c,'p3@127.0.0.1'}),
    stop({d,'p4@127.0.0.1'}),
    stop({e,'p5@127.0.0.1'}).

stop(Name) ->
    if 
        is_tuple(Name) ->
            Name ! stop;
        true ->
            case whereis(Name) of
                undefined ->
                    ok;
                Pid ->
                    Pid ! stop
            end
    end.
    

