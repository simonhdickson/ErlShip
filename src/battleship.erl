-module(battleship).
-export([game/0,game/1,game/2,shoot/2,ship/1]).

game() ->
    game([[{1,2},{2,2}],[{4,5},{4,6}]]).
game(Size, LengthList) ->
    Ships = generateShips(Size, LengthList),
    game(Ships).
game(ShipList) ->
    spawn(?MODULE, ship, [ShipList]).
    
shoot(Pid,Target) ->
    Pid ! {self(), {shoot, Target}},
    receive
        {Pid, Message} -> Message
    end.

ship(ShipList) ->
    receive
        {From, {shoot, Target}} ->
            case anyHit(Target,ShipList) of
                true ->  {Output, NewList} = onCollision(Target,ShipList),
                         From ! {self(), Output},
                         ship(NewList);
                false -> From ! {self(), miss},
                         ship(ShipList)
            end;
        terminate ->
            ok
    end.

onCollision(Target,ShipList) ->
    NewList = deleteHit(Target,ShipList),
    N = length(ShipList),
    case length(NewList) of
        0 -> {{hit,won},[]};
        N -> {{hit},NewList};
        _ -> {{hit,sunk},NewList}
    end.

generateShips(Size,LengthList) ->
    MapFun = fun(X, Ships) -> Ships ++ [generateShip(Size,X,[Ships])] end,
    lists:foldl(MapFun, [], LengthList).

generateShip(Size,Length,ShipList) ->
    Ship = generateShip(Size,Length),
    case lists:any(fun(X) -> anyHit(X,ShipList) end, Ship) of
        true -> generateShip(Size,Length,ShipList);
        false -> Ship
    end.
generateShip(Size,Length) ->
    Horizontal = random:uniform(2) == 1,
    Constant = random:uniform(Size),
    Vary = random:uniform(Size-Length),
    Dots = lists:seq(Vary,Vary+Length-1),
    MapYFun = fun(Y) -> {Constant,Y} end,
    MapXFun = fun(X) -> {X,Constant} end,
    if
        Horizontal -> lists:map(MapYFun,Dots);
        not(Horizontal) -> lists:map(MapXFun,Dots)
    end.

anyHit(_,[]) ->
    false;
anyHit(Target, [H|T]) ->
    case lists:any(fun(X)->X==Target end,H) of
        true -> true;
        false -> anyHit(Target,T)
    end.

deleteHit(Target, List) ->
    deleteHit(Target,List,[]).
deleteHit(_,[],Return) ->
    Return;
deleteHit(Target,[H|T],Return) ->
    case lists:delete(Target,H) of
        [] -> Return ++ T;
        H -> deleteHit(Target,T,[H] ++ Return);
        New -> [New] ++ Return ++ T
    end.