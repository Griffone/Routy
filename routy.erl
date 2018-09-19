-module(routy).

-export([start/2, stop/1, status/1]).

start(Reg, Name) ->
    register(Reg, spawn(fun() -> init(Name) end)).

stop(Node) ->
    Node ! stop,
    unregister(Node).

status(Process) ->
    Process ! {status, self()},
    receive
        {status, {Name, N, Hist, Intf, Table, Map}} ->
            io:format("~w's status:~n", [Process]),
            lists:foreach(fun({String, Item}) ->
                io:format("~-13.s : ~w~n", [String, Item]) end,
                [{"Name", Name},
                {"N", N},
                {"History", Hist},
                {"Interfaces", intf:list(Intf)},
                {"Routing table", Table},
                {"Node map", Map}]),
            ok
    after
        1000 ->
            io:format("~w failed to send status~n", [Process]),
            {error, unresponsive}
    end.

init(Name) ->
    %Intf = intf:add(Name, self(), self(), intf:new()),
    Intf = intf:new(),
    Map = map:new(),
    Table = dijkstra:table(intf:list(Intf), Map),
    Hist = hist:new(Name),
    router(Name, 0, Hist, Intf, Table, Map).

router(Name, N, Hist, Intf, Table, Map) ->
    receive
        % Control message
        % Adds an interface (a link-connection)
        {add, Node, Pid} ->
            Ref = erlang:monitor(process, Pid),
            UpdatedIntf = intf:add(Node, Ref, Pid, Intf),
            router(Name, N, Hist, UpdatedIntf, Table, Map);
        
        % Control message
        % Removes an interface (a link-connection)
        {remove, Node} ->
            {ok, Ref} = intf:ref(Node, Intf),
            erlang:demonitor(Ref),
            UpdatedIntf = intf:remove(Node, Intf),
            router(Name, N, Hist, UpdatedIntf, Table, Map);
        
        % Control message
        % Signals that we should update our routing table
        % For a real router would be done automatically at some semi-regular interval
        update ->
            Interfaces = intf:list(Intf),
            UpdatedMap = map:update(Name, Interfaces, Map),
            UpdatedTable = dijkstra:table(Interfaces, UpdatedMap),
            router(Name, N, Hist, Intf, UpdatedTable, UpdatedMap);
        
        % Control message
        % Manual broadcast
        % For a real router this would be automatic, when the network state changes significantly
        broadcast ->
            Message = {links, Name, N, intf:list(Intf)},
            intf:broadcast(Message, Intf),
            router(Name, N+1, Hist, Intf, Table, Map);

        % Control message
        % Signals to return current state for nice print
        {status, From} ->
            From ! {status, {Name, N, Hist, Intf, Table, Map}},
            router(Name, N, Hist, Intf, Table, Map);
        
        % Control message
        % Stop the router's execution
        stop ->
            ok;

        % Control message
        % Send a message to a given destination
        % Can be used without knowing the local router name
        {send, To, Message} ->
            self() ! {route, To, Name, Message},
            router(Name, N, Hist, Intf, Table, Map);
        
        % Erlang process monitor message
        % Means that the monitored process is down
        {'DOWN', Ref, process, _, _} ->
            {ok, Down} = intf:name(Ref, Intf),
            io:format("~w: exit received from ~w~n", [Name, Down]),
            UpdatedIntf = intf:remove(Down, Intf),
            router(Name, N, Hist, UpdatedIntf, Table, Map);

        % Link-state message
        % The self-managing network part
        {links, Node, R, Links} ->
            case hist:update(Node, R, Hist) of
                {new, UpdatedHist} ->
                    intf:broadcast({links, Node, R, Links}, Intf),
                    UpdatedMap = map:update(Node, Links, Map),
                    router(Name, N, UpdatedHist, Intf, Table, UpdatedMap);
                old ->
                    router(Name, N, Hist, Intf, Table, Map)
            end;
        
        % Message routing
        % Message arrived at its destination
        {route, Name, From, Message} ->
            io:format("~w: received message (~w) from ~w~n", [Name, Message, From]),
            router(Name, N, Hist, Intf, Table, Map);
        
        % Message routing
        % Message should be forwarded
        {route, To, From, Message} ->
            io:format("~w: forwarding message (~w) to ~w~n", [Name, Message, To]),
            case dijkstra:route(To, Table) of
                {ok, Gateway} ->
                    case intf:lookup(Gateway, Intf) of
                        {ok, Pid} ->
                            Pid ! {route, To, From, Message};
                        notfound ->
                            ok
                    end;
                notfound ->
                    ok
            end,
            router(Name, N, Hist, Intf, Table, Map)
        
    end.