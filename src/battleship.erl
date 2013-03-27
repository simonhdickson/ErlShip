-module(battleship).
-export([start_cpu_game/2, game/0, game/1, game/2, ship/3]).

start_cpu_game(Size, LengthList) ->
  game(Size, LengthList) ! {game(Size, LengthList), {start, Size}}.
game() ->
  random:seed(now()),
  start_cpu_game(10, [1, 2, 3, 4]).
game(Size, LengthList) ->
  Ships = generateShips(Size, LengthList),
  game({Ships, Size}).
game({ShipList, Size}) ->
  spawn(?MODULE, ship, [ShipList, [], {Size, 0}]).

ship(ShipList, PreviousShots, Size) ->
  receive
    {From, {shoot, Target}} ->
      case anyHit(Target, ShipList) of
        true -> {Output, NewList} = onCollision(Target, ShipList),
          From ! {self(), Output},
          ship(NewList, PreviousShots, Size);
        false -> From ! {self(), miss},
          ship(ShipList, PreviousShots, Size)
      end;

    {From, {start, NewTarget}} ->
      {MySize, _} = Size,
      From ! {self(), {confirm_start,MySize}},
      ship(ShipList, PreviousShots, {MySize, NewTarget});

    {From, {confirm_start, NewTarget}} ->
      {MySize, _} = Size,
      From ! {self(), your_turn},
      ship(ShipList, PreviousShots, {MySize, NewTarget});

    {From, Message} ->
      case aiTurn(Message,{ShipList, PreviousShots, Size}) of
        quit -> From ! self() ! terminate;
        {shoot,Target} -> From ! {self(), {shoot,Target}},
          ship(ShipList, PreviousShots ++ [Target], Size);
        ok -> From ! {self(), your_turn},
          ship(ShipList, PreviousShots, Size)
      end;

    terminate ->
      ok
  end.

aiTurn(won,_)->
  io:format('woo i winz!'), quit;
aiTurn(your_turn, {_,PreviousShots,{_,TargetSize}}) ->
  {shoot, makeShot(TargetSize,PreviousShots)};
aiTurn(hit,_)-> io:fwrite("~62p~n", [hit]);
aiTurn(sunk,_)-> io:fwrite("~62p~n", [sunk]);
aiTurn(_,_) -> ok.

makeShot(Size, PreviousShots) ->
  Shot = {random:uniform(Size), random:uniform(Size)},
  case lists:any(fun(X) -> X == Shot end, PreviousShots) of
    true -> makeShot(Size, PreviousShots);
    false -> Shot
  end.

onCollision(Target, ShipList) ->
  NewList = deleteHit(Target, ShipList),
  N = length(ShipList),
  case length(NewList) of
    0 -> {won, []};
    N -> {hit, NewList};
    _ -> {sunk, NewList}
  end.

generateShips(Size, LengthList) ->
  MapFun = fun(X, Ships) -> Ships ++ [generateShip(Size, X, [Ships])] end,
  lists:foldl(MapFun, [], LengthList).

generateShip(Size, Length, ShipList) ->
  Ship = generateShip(Size, Length),
  case lists:any(fun(X) -> anyHit(X, ShipList) end, Ship) of
    true -> generateShip(Size, Length, ShipList);
    false -> Ship
  end.
generateShip(Size, Length) ->
  Horizontal = random:uniform(2) == 1,
  Constant = random:uniform(Size),
  Vary = random:uniform(Size - Length),
  Dots = lists:seq(Vary, Vary + Length - 1),
  if
    Horizontal -> lists:map(fun(Y) -> {Constant, Y} end, Dots);
    not(Horizontal) -> lists:map(fun(X) -> {X, Constant} end, Dots)
  end.

anyHit(_, []) -> false;
anyHit(Target, [H | T]) ->
  case lists:any(fun(X) -> X == Target end, H) of
    true -> true;
    false -> anyHit(Target, T)
  end.

deleteHit(Target, List) -> deleteHit(Target, List, []).
deleteHit(_, [], Return) -> Return;
deleteHit(Target, [H | T], Return) ->
  case lists:delete(Target, H) of
    [] -> Return ++ T;
    H -> deleteHit(Target, T, [H] ++ Return);
    New -> [New] ++ Return ++ T
  end.