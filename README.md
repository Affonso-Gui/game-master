# Game-master

Game-master is an esoteric programming language based on Magic the Gathering and inspired by Shakespeare Language, created by Guilherme Affonso in 2017. It is designed to make programs look like card game battles, or rather give printable results to existing (possible) battles.

Combining game-master with real game engines would make software co-creation as fun as playing card games, and could also be used to boost the original games by adding restrictions or new objectives, like *The first player to complete Hello World Program wins the game* !

Below is a fragment of Hello World program, that uses only real Magic the Gathering cards on an epic battle that goes through 13 turns and 330 lines.
```
______

TURN 5

PLAYER 1
> Summon "Arashin Cleric".
> Trigger "Arashin Cleric" ability.
> Summon "Skittering Skirge".
> Cast "Shock" on Player 2.
> End turn.

PLAYER 2
> Summon "Arashin Cleric".
> Trigger "Arashin Cleric" ability.
> Combat "Palace Familiar" "Skittering Skirge".
> Trigger "Palace Familiar" ability.
> Summon "Frostling".
> Activate "Frostling" ability targeting "Skittering Skirge".
> Summon "Armored Pegasus".
> End turn.

______
```

## Startup
Compile with:
```
sudo apt-get install clisp
clisp -q -c main.l -o ~/.game-master.fas
```
Then start with `clisp -q -q -i ~/.game-master.fas -repl` or with `game-master` script.

## How it works

Each player starts with 20 life points, and take turns casting spells, summoning creatures and attacking with them in order to reduce the opponent's life to zero. A player can only take actions during his/her turn, and declaration of attacking creatures may be done at any time of the turn, choosing wich player/creature to attack as well. Something like Shadow Verse, except damage dealt to creatures fades away at the end of every turn. Seems just like a normal card game, right?

The difference is that each player also holds a variable `val`, that starts at zero and gets changed or printed during the game, according to the following:

-**Damage:**
Dealing X damage to another player adds 2^X to `val`, taking Y damage subtracts Y from `val`.

-**Life gain:**
Gaining X life multiplies `val` by 2^X.

-**Life pay/loss:**
Paying or losing (exept by damage) X life multiplies `val` by -X.

-**Sacrifice:**
Sacrificing a creature sets `val` to zero.

-**Draw:**
Drawing X cards prints character stored at `val` X times, according to ASCII code.

-**Discard:**
Discarding X cards prints number stored at `val` X times.

## Basic Syntax

Game-master is stablished by definig three macro characters:

`_`**:** Used to define players, cards and turns, witch are written in blocks between `_`. The first word of the block is the identifier, and examples of how to write it can be found in `hello.gm`.

`>`**:** Used for turn actions and interpreter mode interaction. For instance:
```
___
TURN 1

PLAYER 1
> Cast "Shock" on Player 1.
> End turn.
___
```
Is equivalent to
```
> Turn 1
> Start Player 1's turn.
> Cast "Shock" on Player 1.
> End turn.
```
Except that in the second case the user can see the results and game state line-by-line.

`<`**:** Used to print information about the game status. For instance, `< LIFE` prints the life of each player, and `< VAL` prints each player's `val` variable.


## What is supported:

-**Card types:**
Creatures and Spells.

-**Effects:**
Damage, gain life, pay and lose life, draw, discard, destroy, exile, sacrifice. All others effects are ignored, and one sentence may only have one effect.

-**Turn actions:** Summon, Cast, Attack, Activate, Turn (set turn number), Start turn, End turn. Card names must be written between `"`

-`<`**:** Cards, Players, Player names, Turn, Creatures, Life, Field, Mana, Val

## Other commands:
Any clisp command without `_`, `>`, and `<` works normally on game-master. Specially useful ones are `quit` for reseting errors and `(quit)` or `(exit)` for exiting the interpretor.
