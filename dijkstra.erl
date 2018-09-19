-module(dijkstra).

-export([entry/2, replace/4, update/4, iterate/3, table/2, route/2]).
%-export([table/2, route/2]).

-import(map, [reachable/2, all_nodes/1]).

% Construct a routing table.
table(Gateways, Map) when is_list(Gateways); is_map(Map) ->
    Nodes = map:all_nodes(Map),
    SortedNodes = lists:foldl(fun(Node, List) -> [{Node, inf, unknown} | List] end,
        [], Nodes),
    Sorted = lists:foldl(fun(Gateway, List) -> update(Gateway, 0, Gateway, List) end,
        SortedNodes, Gateways),
    iterate(Sorted, Map, #{}).

% Return the gateway we should send a message to Node, given the Table.
route(Node, Table) ->
    Gateway = maps:get(Node, Table, notfound),
    case Gateway of
        notfound ->
            notfound;
        _ ->
            {ok, Gateway}
    end.

% Returns the length of the shortest path to the node or 0 if the node is not found.
entry(_, []) ->
    0;
entry(Node, [{Node, N, _} | _]) ->
    N;
entry(Node, [_ | Rest]) ->
    entry(Node, Rest).

% Replace the entry for the Node in Sorted with a new length N and Gateway. Result should be sorted.
replace(Node, N, Gateway, Sorted) ->
    replace(Node, N, Gateway, [], Sorted).
replace(Node, N, Gateway, Processed, [{Node, OldN, _} | Rest]) ->
    if
        N >= OldN ->
            reassemble(Processed, rinject(Node, N, Gateway, Rest));
        true ->
            reassemble(linject(Node, N, Gateway, Processed), Rest)
    end;
replace(Node, N, Gateway, Processed, [Item | Rest]) ->
    replace(Node, N, Gateway, [Item | Processed], Rest);
replace(_, _, _, _, []) ->
    error.

% Update an entry if it's more efficient.
update(Node, N, Gateway, Sorted) ->
    OldN = entry(Node, Sorted),
    if
        OldN > N ->
            replace(Node, N, Gateway, Sorted);
        % This includes the case of Node not being found, as zero is smallest number.
        true ->
            Sorted
    end.

% Iterate typical dijkstra pathfinding.
iterate([], _, Table) ->
    Table;
iterate([{_, inf, _} | _], _, Table) ->
    Table;
iterate([{Node, N, Gateway} | Rest], Map, Table) ->
    Reachable = map:reachable(Node, Map),
    Updated = lists:foldl(fun(Item, List) -> update(Item, N + 1, Gateway, List) end, Rest, Reachable),
    iterate(Updated, Map, Table#{Node => Gateway}).

reassemble([], Right) ->
    Right;
reassemble([Item | Rest], Right) ->
    reassemble(Rest, [Item | Right]).

rinject(Node, N, Gateway, List) ->
    case List of
        [] ->
            [{Node, N, Gateway}];
        [{_, RN, _}] when RN >= N ->
            [{Node, N, Gateway} | List];
        [Item | Rest] ->
            [Item | rinject(Node, N, Gateway, Rest)]
    end.

linject(Node, N, Gateway, List) ->
    case List of
        [] ->
            [{Node, N, Gateway}];
        [{_, LN, _}] when LN =< N ->
            [{Node, N, Gateway} | List];
        [Item | Rest] ->
            [Item | linject(Node, N, Gateway, Rest)]
    end.