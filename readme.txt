Toledo Atomchess 6502 for Atari VCS/2600
(c) Copyright 2017 Oscar Toledo Gutierrez
http://nanochess.org/

Latest source at http://github.com/nanochess

This is a port of my Toledo Atomchess x86 game to 6502 code playable with Atari VCS/2600.

Besides the small AI it includes a graphical board and selection of pieces with joystick plus some wacko sounds.

Don't expect the greatest visuals, I coded this in a night for the Hackaday 1K contest https://hackaday.io/contest/18215-the-1kb-challenge

Assemble with dasm from http://dasm-dillon.sourceforge.net/ 
Tested with Stella from http://stella.sourceforge.net/
Tested with real Atari 2600 using Harmony cart.
Tested with online emulation from http://8bitworkshop.com/
Demostration video at https://www.youtube.com/watch?v=_Du4krvIl7o

Misfeatures:

* Computer plays legal basic chess movements :)
* Move with joystick, push button to select, push button to drop.
* Promotion of pawns only to queen.
* No castling
* No enpassant
* No move validation (if you push button in the wrong square... well you can imagine it)


Playing it in real hardware:

Using a Harmony cartridge you can load the 1K ROM into it an insert it to play over a real Atari VCS/2600.

For some older versions of Harmony cartridge you need to replicate the 1K ROM to 4K, using this series of commands:

    COPY /B atomchess.bin+atomchess.bin+atomchess.bin+atomchess.bin atom4k.bin

Or for Mac OS X:

    cat toledo_atomchess_6502.bin toledo_atomchess_6502.bin toledo_atomchess_6502.bin toledo_atomchess_6502.bin >atomchess4k.bin


Long description:

The 719 lines of assembler program (664 without comments) are separated in three well defined sections: the initialization, the display/controller and the AI engine.

Given the primitive Atari VCS/2600 doesn't provide routines or BIOS ROM, it's needed to take care of initializing the processor, reset all video, audio and controller registers.

It only has 128 bytes of RAM memory, of these 78 are allocated to the chessboard and 38 for stack (AI recursive search with 2-ply depth), remains 12 bytes for housekeeping.

The video display processor (TIA) works displaying line-by-line based on input registers, this means the CPU should update video register per each scanline in tight timings and turn on/off the vertical retrace.

If the CPU doesn't update the TIA in time, essentially we lose TV synchro, but to simplify the game, the AI engine works without updating the TIA.

The TIA can show only a 40 columns playfield (background), 2 bitmap objects per line (each one can be repeated upto 3 times) and 3 pixel objects. The playfield is used for the chessboard squares, given only 2 different bitmap objects can be shown in a line the board and it have "big" graphics, it shows alternate columns in 4 frames, this sustains for 15hz flicker.

Even if pixel objects are a single pixel (but can be "fat") these can be draw over several lines, making a big object like the cursor which uses missile 0.

Of the 1024 bytes, the AI engine uses 429 bytes (initialization+engine+tables), the repetitive nature of the TIA display occupies the remaining bytes. Still remains 30 bytes free for improvements.

