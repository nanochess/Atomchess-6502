Toledo Atomchess 6502 for Atari VCS/2600
(c) Copyright 2017 Oscar Toledo Gutierrez
http://nanochess.org/

This is a port of my Toledo Atomchess x86 game to 6502 code playable with Atari VCS/2600.

It includes a graphical board and selection of pieces with joystick plus some wacko sounds.

Don't expect the greatest visuals, I coded this in a night for the Hackaday 1K contest https://hackaday.io/contest/18215-the-1kb-challenge

Assemble with dasm from http://dasm-dillon.sourceforge.net/ 
Tested with Stella from http://stella.sourceforge.net/

Misfeatures:

* Computer plays legal basic chess movements :)
* Move with joystick, push button to select, push button to drop.
* Promotion of pawns only to queen.
* No castling
* No enpassant
* No move validation (if you push button in the wrong square... well you can imagine it)

Latest source at http://github.com/nanochess

