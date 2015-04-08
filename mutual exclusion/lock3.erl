-module(lock3).
-export([start/1]).

start(MyId) ->
    spawn(fun() -> init(MyId) end).

init(MyId) ->
    MyClock = 0,
    receive
        {peers, Nodes} ->
            open(Nodes,MyId,MyClock);
        stop ->
            ok
    end.

open(Nodes, MyId,MyClock) ->
    receive
        {take, Master} ->
            Refs = requests(Nodes, MyId,MyClock),
            wait(Nodes, Master, Refs, [], MyId, MyClock, MyClock);
        {request, From,  Ref, _, RemoteClock} ->
            NewClock = lists:max([MyClock,RemoteClock]) + 1,
            From ! {ok, Ref, NewClock},
            open(Nodes, MyId, NewClock);
        stop ->
            ok
    end.

requests(Nodes, MyId, MyClock) ->
    lists:map(
      fun(P) -> 
        R = make_ref(), 
        P ! {request, self(), R, MyId, MyClock}, 
        R 
      end, 
      Nodes).

wait(Nodes, Master, [], Waiting, MyId, MyClock, _) ->
    Master ! taken,
    held(Nodes, Waiting,MyId,MyClock);
    
wait(Nodes, Master, Refs, Waiting, MyId, MyClock, TimeStamp) ->
    receive
        {request, From, Ref, OtherId, RemoteClock} ->
            NewClock = lists:max([MyClock,RemoteClock]) + 1,
    	    if 
                RemoteClock < TimeStamp ->
                    From ! {ok, Ref, NewClock}, 
                    wait(Nodes, Master, Refs, Waiting, MyId, NewClock, TimeStamp);  

                RemoteClock == TimeStamp ->
                    if
                        OtherId < MyId ->
                  	        From ! {ok, Ref, NewClock},
                	        wait(Nodes, Master, Refs, Waiting, MyId, NewClock, TimeStamp);	      
            	        true -> 
                            wait(Nodes, Master, Refs, [{From, Ref}|Waiting], MyId, NewClock, TimeStamp)
                    end;
                true ->
                    wait(Nodes, Master, Refs, [{From, Ref}|Waiting], MyId, NewClock, TimeStamp)
    	    end;

        {ok, Ref, RemoteClock} ->
            NewClock = lists:max([MyClock,RemoteClock]) + 1,
            NewRefs = lists:delete(Ref, Refs),
            wait(Nodes, Master, NewRefs, Waiting, MyId, NewClock, TimeStamp);

        release ->
            ok(Waiting, MyClock),            
            open(Nodes, MyId, MyClock)
    end.

ok(Waiting, MyClock) ->
    lists:map(
      fun({F,R}) -> 
        F ! {ok, R, MyClock} 
      end, 
      Waiting).

held(Nodes, Waiting, MyId, MyClock) ->
    receive
        {request, From, Ref, _, RemoteClock} ->
            NewClock = lists:max([MyClock,RemoteClock]) + 1,
            held(Nodes, [{From, Ref}|Waiting],MyId, NewClock);
        release ->
            ok(Waiting, MyClock),
            open(Nodes,MyId, MyClock)
    end.
