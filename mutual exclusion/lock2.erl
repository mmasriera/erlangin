-module(lock2).
-export([start/1]).

start(MyId) ->
    spawn(fun() -> init(MyId) end).

init(MyId) ->
    receive
        {peers, Nodes} ->
            open(Nodes, MyId);
        stop ->
            ok
    end.

open(Nodes, MyId) ->
    receive
        {take, Master} ->
            Refs = requests(Nodes, MyId),
            wait(Nodes, Master, Refs, [], MyId);
        {request, From,  Ref, _} ->
            From ! {ok, Ref},
            open(Nodes, MyId);
        stop ->
            ok
    end.

requests(Nodes, MyId) ->
    lists:map(
      fun(P) -> 
        R = make_ref(), 
        P ! {request, self(), R, MyId}, 
        R 
      end, 
      Nodes).

wait(Nodes, Master, [], Waiting, MyId) ->
    Master ! taken,
    held(Nodes, Waiting, MyId);
    
wait(Nodes, Master, Refs, Waiting, MyId) ->
    receive
        {request, From, Ref, OtherId} ->
            if 
                OtherId < MyId ->
                    From ! {ok, Ref},
                    NewRef = make_ref(),
                    From ! {request, self(), NewRef, MyId}, 
                    wait(Nodes, Master, [NewRef | Refs], Waiting, MyId);
                true -> 
                    wait(Nodes, Master, Refs, [{From, Ref}|Waiting], MyId)
            end;
        {ok, Ref} ->
            NewRefs = lists:delete(Ref, Refs),
            wait(Nodes, Master, NewRefs, Waiting, MyId);
        release ->
            ok(Waiting),            
            open(Nodes, MyId)
    end.

ok(Waiting) ->
    lists:map(
      fun({F,R}) -> 
        F ! {ok, R} 
      end, 
      Waiting).

held(Nodes, Waiting, MyId) ->
    receive
        {request, From, Ref, _} ->
            held(Nodes, [{From, Ref}|Waiting], MyId);
        release ->
            ok(Waiting),
            open(Nodes, MyId)
    end.
