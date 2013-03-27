ErlShip
=======

Battleships written in Erlang

Easiest way to get this going, load up terminal:

	cd(/checkoutLocation).
 	c(battleship).

To set the game going (where 10 is the size of the grid, and [2,3,5] are the sizes of the ships.)

	Game = battleship:game(10,[2,3,5]).

To make a shot:

	battleship:shoot(Game, {1,2}).

Have fun!

AI
==

Just added AI support so if you run:

	battleship:game().
	
Two AIs will play each other.

Even though it would be far more useful to be able to play against the AI, that doesn't actually
work... (I just thought it was cooler to have two AIs...)
