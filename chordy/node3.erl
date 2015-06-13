-module(node3).
-export([start/1, start/2]).

-define(Stabilize, 1000).
-define(Timeout, 5000).

start(MyKey) ->
    start(MyKey, nil).

start(MyKey, PeerPid) ->
    timer:start(),
    spawn(fun() -> init(MyKey, PeerPid) end).

init(MyKey, PeerPid) ->
    Predecessor = nil,
    {ok, Successor} = connect(MyKey, PeerPid),
    schedule_stabilize(),        
    node(MyKey, Predecessor, Successor, nil, storage:create()).

connect(MyKey, nil) ->
    {ok, {MyKey, nil, self()}};    
connect(_, PeerPid) ->
    Qref = make_ref(),
    PeerPid ! {key, Qref, self()},
    receive
        {Qref, Skey} ->
            Ref = monit(PeerPid),
            {ok, {Skey, Ref, PeerPid}}    
    after ?Timeout ->
        io:format("Timeout: no response from ~w~n", [PeerPid])
    end.

schedule_stabilize() ->
    timer:send_interval(?Stabilize, self(), stabilize).

node(MyKey, Predecessor, Successor, Next, Store) ->
    receive 
        {key, Qref, PeerPid} ->
            PeerPid ! {Qref, MyKey},
            node(MyKey, Predecessor, Successor, Next, Store);
        {notify, New} ->
            {Pred, Stre} = notify(New, MyKey, Predecessor, Store),
            node(MyKey, Pred, Successor, Next, Stre);
        {request, Peer} ->
            request(Peer, Predecessor, Successor),
            node(MyKey, Predecessor, Successor, Next, Store);
        {status, Pred, Nx} ->
            {Succ, Nxt} = stabilize(Pred, Nx, MyKey, Successor),
            node(MyKey, Predecessor, Succ, Nxt, Store);
        stabilize ->
            stabilize(Successor),
            node(MyKey, Predecessor, Successor, Next, Store);
        probe ->
            create_probe(MyKey, Successor, Store),
            node(MyKey, Predecessor, Successor, Next, Store);
        {probe, MyKey, Nodes, T} ->
            remove_probe(MyKey, Nodes, T, Store),
            node(MyKey, Predecessor, Successor, Next, Store);
        {probe, RefKey, Nodes, T} ->
            forward_probe(RefKey, [MyKey|Nodes], T, Successor, Store),
            node(MyKey, Predecessor, Successor, Next, Store);
        {add, Key, Value, Qref, Client} ->
            Added = add(Key, Value, Qref, Client,
            MyKey, Predecessor, Successor, Store),
            node(MyKey, Predecessor, Successor, Next, Added);
        {lookup, Key, Qref, Client} ->
            lookup(Key, Qref, Client, MyKey, Predecessor, Successor, Store),
            node(MyKey, Predecessor, Successor, Next, Store);
        {handover, Elements} ->
            Merged = storage:merge(Store, Elements),
            node(MyKey, Predecessor, Successor, Next, Merged);
        {'DOWN', Ref, process, _, _} ->
            {Pred, Succ, Nxt} = down(Ref, Predecessor, Successor, Next),
            node(MyKey, Pred, Succ, Nxt, Store);
        stop ->
            exit(self())
   end.

   
stabilize(Pred, Nx, MyKey, Successor) ->
  {Skey, Sref, Spid} = Successor,
  case Pred of
      nil ->
          Spid ! {notify, {MyKey, nil, self()}},
          {Successor, Nx};
      {MyKey, _, _} ->
          {Successor, Nx};
      {Skey, _, _} ->
          Spid ! {notify, {MyKey, nil, self()}},
          {Successor, Nx};
      {Xkey, _, Xpid} ->
            case key:between(Xkey, MyKey, Skey) of
                true ->
                    Xref = monit(Xpid),
                    demonit(Sref),
                    self() ! stabilize,
                    {{Xkey, Xref, Xpid}, Successor};
                false ->
                    Spid ! {notify, {MyKey, nil, self()}},
                    {Successor, Nx}
            end
    end.

stabilize({_, _, Spid}) ->
    Spid ! {request, self()}.

request(Peer, Predecessor, {Skey, Sref, Spid}) ->
    case Predecessor of
        nil ->
            Peer ! {status, nil, {Skey, Sref, Spid}};
        {Pkey, Pref, Ppid} ->
            Peer ! {status, {Pkey, Pref, Ppid}, {Skey, Sref, Spid}}
    end.

notify({Nkey, _, Npid}, MyKey, Predecessor, Store) ->
    case Predecessor of
        nil ->
            NRef = monit(Npid),
            Keep = handover(Store, MyKey, Nkey, Npid),
            {{Nkey, NRef, Npid}, Keep};
        {Pkey, Pref, _} ->
            case key:between(Nkey, Pkey, MyKey) of
                true ->
                    demonit(Pref),
                    Nref = monit(Npid),
                    Keep = handover(Store, MyKey, Nkey, Npid),
                    {{Nkey, Nref, Npid}, Keep};
                false -> 
                    monit(Npid),
                    {Predecessor, Store}
            end
    end.

create_probe(MyKey, {_, _, Spid}, Store) ->
    Spid ! {probe, MyKey, [MyKey], erlang:now()},
    io:format("Create probe ~w! Store ~w~n", [MyKey, Store]).
	
remove_probe(MyKey, Nodes, T, Store) ->
    Time = timer:now_diff(erlang:now(), T) div 1000,
    io:format("Received probe ~w in ~w ms Ring: ~w Store ~w~n", [MyKey, Time, Nodes, Store]).
	
forward_probe(RefKey, Nodes, T, {_, _, Spid}, Store) ->
    Spid ! {probe, RefKey, Nodes, T},
    io:format("Forward probe ~w! Store ~w~n", [RefKey, Store]).

add(Key, Value, Qref, Client, MyKey, {Pkey, _, _}, {_, _, Spid}, Store) ->
    case key:between(Key, Pkey, MyKey) of
        true ->
            Added = storage:add(Key, Value, Store),
            Client ! {Qref, ok},
            Added;
        false ->
            Spid ! {add, Key, Value, Qref, Client},
            Store
    end.
    
lookup(Key, Qref, Client, MyKey, {Pkey, _, _}, {_, _, Spid}, Store) ->
    case key:between(Key, Pkey, MyKey) of
        true ->
            Result = storage:lookup(Key, Store),
            Client ! {Qref, Result};
        false ->
            Spid ! {lookup, Key, Qref, Client}
    end.
    
handover(Store, MyKey, Nkey, Npid) ->
    {Keep, Leave} = storage:split(MyKey, Nkey, Store),
    Npid ! {handover, Leave},
    Keep.

monit(Pid) ->
    erlang:monitor(process, Pid).

demonit(nil) ->
    ok;
demonit(MonitorRef) ->
    erlang:demonitor(MonitorRef, [flush]).

down(Ref, {_, Ref, _}, Successor, Next) ->
    {nil, Successor, Next};

down(Ref, Predecessor, {_, Ref, _}, {Nkey, _, Npid}) ->
    self() ! stabilize,
    Nref = monit(Npid),
    {Predecessor, {Nkey, Nref, Npid}, nil}.