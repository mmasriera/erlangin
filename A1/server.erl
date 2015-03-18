-module(server).
%% Exported Functions
-export([start/0]).

%% API Functions
start () ->
    ServerPid = spawn (fun () -> 
	process_requests([]) 
    end),
    register(myserver, ServerPid).

    
process_requests (Clients) ->
    receive
        {client_join_req, Name, From} ->
            NewClients = [From | Clients], 
            io:format("client joined ~n"),
            broadcast(NewClients, {join, Name}),
            process_requests(NewClients); 
        {client_leave_req, Name, From} ->
            NewClients = lists:delete(From, Clients), 
            io:format("client deleted~n"),
            broadcast(Clients, {leave, Name}), 
            From ! exit,
            process_requests(NewClients); 
        {send, Name, Text} ->
            broadcast(Clients, {message, Name, Text}), 
            io:format("message~n"),
            process_requests(Clients);
        disconnect ->
            unregister(myserver)
    end.

%% Local Functions 
broadcast (PeerList, Message) ->
    	Fun = fun (Peer) -> 
    		Peer ! Message 
	end,
    	lists:map(Fun, PeerList).
