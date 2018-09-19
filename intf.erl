-module(intf).

-export([new/0, add/4, remove/2, lookup/2, ref/2, name/2, list/1, broadcast/2]).

% Return new empty set of interfaces.
new() ->
    #{}.

% Add a new entry to the set and return new set of interfaces.
add(Name, Reference, Pid, Interfaces) ->
    Interfaces#{Name => {Reference, Pid}}.

% Remove an entry from the interface.
remove(Name, Interfaces) ->
    maps:remove(Name, Interfaces).

% Get identifier of the interface.
lookup(Name, Interfaces) ->
    Element = maps:get(Name, Interfaces, notfound),
    case Element of
        {_, Pid} ->
            {ok, Pid};
        _ ->
            notfound
    end.

ref(Name, Interfaces) ->
    Element = maps:get(Name, Interfaces, notfound),
    case Element of
        {Reference, _} ->
            {ok, Reference};
        _ ->
            notfound
    end.

name(Reference, Interfaces) ->
    Filtered = maps:filter(fun(_, {VR, _}) -> VR == Reference end, Interfaces),
    case maps:keys(Filtered) of
        [Name] ->
            {ok, Name};
        _ ->
            notfound
    end.

list(Interfaces) ->
    maps:keys(Interfaces).

broadcast(Message, Interfaces) ->
    Values = maps:values(Interfaces),
    lists:foreach(fun({_, Pid}) -> Pid ! Message end, Values).