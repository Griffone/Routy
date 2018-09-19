-module(hist).

-export([new/1, update/3]).

% Messages coming from Name are always old
new(Name) ->
    {Name, #{}}.

% Check if a message is old
update(Node, _, {Node, _}) ->
    old;
update(Node, N, {Ignore, Map}) ->
    OldN = maps:get(Node, Map, -1),
    if
        OldN < N ->
            {new, {Ignore, Map#{Node => N}}};
        true ->
            old
    end.