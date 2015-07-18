-module(cache).
-export([lookup/2, new/0, add/4, remove/2]).

new() ->
  [].

lookup(Name, Cache) ->
  case lists:keyfind(Name, 1, Cache) of
    false ->
      unknown;
    {Key, Value, Expire} ->
      Valid = time:valid(Expire, time:now()),
      if Valid ->
        Value;
      true ->
        invalid
    end
  end.
  
add(Domain, Expire, Reply, Updated) ->
  lists:keystore(Domain, 1, Updated, {Domain, Reply, Expire}).
  
remove(Name, Cache) ->
  lists:keydelete(Name, 1, Cache).