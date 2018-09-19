-module(map).

-export([new/0, update/3, reachable/2, all_nodes/1]).

% Return a new empty map.
new() ->
    #{}.

% Update map to reflect that Node has connections to provided links.
update(Node, Links, Map) when is_list(Links); is_map(Map) ->
    Map#{Node => Links}.

% Get a list of nodes directly reachable from Node.
reachable(Node, Map) ->
    maps:get(Node, Map, []).

% Get a list of all nodes in the network.
all_nodes(Map) when is_map(Map) ->
    Set = sets:from_list(maps:keys(Map)),
    all_nodes(Set, maps:values(Map)).
all_nodes(Set, []) ->
    sets:to_list(Set);
all_nodes(Set, [SubList | Rest]) ->
    USet = sets:union(Set, sets:from_list(SubList)),
    all_nodes(USet, Rest).