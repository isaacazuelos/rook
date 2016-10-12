# Rook

A chess game for human players.

This is a for-fun project I've been slowly picking away at since 2014. 

## What it will do

The goal is to allow for two human players to play a game of chess in full
accordance with [FIDE rules][rules] [PDF].

Real-world chess notation(s) will be supported.

Game playback from PGN might be added.

[rules]: https://www.fide.com/FIDE/handbook/LawsOfChess.pdf

### What it won't do

A chess AI is not planned.

There will not be a GUI — `rook` will be strictly command-line.

## Requirements and Installation

Installation is handled by [`stack`][stack].

[stack]: https://github.com/commercialhaskell/stack

This includes development stuff like dependency fetching and test running.

## Usage

There isn't anything usable yet.

## Thanks

* The [Chess Programming Wiki](https://chessprogramming.wikispaces.com)
* [FIDE][rules] for a pretty great document outlining the rules.
* [Chess.com](http://chess.com) for making me suck slightly less at the game.

This is also a rewrite of a chess engine ([`SlowChess`][SlowChess]) I started
back in 2014. At the time I was a student who didn't really *get* Monads. I
had figured it would take me about a month.

[SlowChess]: https://github.com/isaacazuelos/SlowChess

## License

This project is [MIT][] Licensed. See the included `LICENSE` file.

[MIT]: https://opensource.org/licenses/MIT
