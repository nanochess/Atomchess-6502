Toledo Atomchess 6502 for Atari VCS/2600
(c) Copyright 2017 Oscar Toledo Gutierrez
http://nanochess.org/

Latest source at http://github.com/nanochess

This is a port of my Toledo Atomchess x86 game to 6502 code playable with Atari VCS/2600.

Besides the small AI it includes a graphical board and selection of pieces with joystick plus some wacko sounds.

Don't expect the greatest visuals, I coded this in a night for the Hackaday 1K contest https://hackaday.io/contest/18215-the-1kb-challenge

Assemble with dasm from http://dasm-dillon.sourceforge.net/ 
Tested with Stella from https://stella-emu.github.io/
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
* Can be assembled also for visual6502.org although too slow for even playing.

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

The TIA can show only a 40 columns playfield (background), 2 bitmap objects per line (each one can be repeated upto 3 times) and 3 pixel objects.

The playfield is used to display the chessboard checkered pattern. Given there are only available 2 bitmap objects, the previous version of Atomchess displayed two columns of board on each frame, so 4 frames were needed to show the full chessboard, given a very intense 15hz flicker.

I've redesigned the display to use "venetian blinds" a technique common to Atari games, but I had a very difficult problem: it was developed for small repeated bitmaps, I developed many versions of this code trying to solve it, the TIA allows for repositioning the bitmap in the same scanline where it's draw but it wouldn't show again till an internal display counter has reached 160 pixels, so I solved it in 3 scanlines: 1. repositioning in one scan line, 2. displaying two pieces and moving to right (the only movement possible that complies with the condition of internal counter reaching 160 pixels), 3. displaying two pieces and repeating.

Even if pixel objects are a single pixel (but can be "fat") these can be draw over several lines, making a big object like the cursor which uses missile 0.

Of the 1024 bytes, the AI engine uses 429 bytes (initialization+engine+tables), the repetitive nature of the TIA display occupies the remaining bytes. Still remains 7 bytes free for improvements.

Just as a note, the Atari Flashback Portable doesn't support the new trick in its emulator and can only display the previous version (search for Jan/16/2017 in commits)


Code for Visual6502.ORG:

This a 596 bytes port, cut and paste in your browser address bar.

http://www.visual6502.org/JSSim?graphics=false&headlesssteps=1000&r=0100&a=0100&d=78d8a2ff9aa900a2809500e8e08cd0f9aaa008a900958ce888d0f8a907958ce8958ce8e050d0eaaabd2703958c090895d2f696a90995c8ca10ee4a85878588209902200103b98c002908f0f3200103b98c002908d0f6207c012099022098014c3f01e682c683d058e8e04ed03368a868bae0fdd022a680e0d3301caab58cc901f004c909d00ac00a9004c04690024905998c00a900958c60a9c085804848a200b58cf0c44581c907b0bec901d006a481f002a900a86903290c8583b9350385828685a482b93c03186585c94eb0948585c010a8b98c00f0269006a583c9039088b98c00458138e909c906b01ac905d0236868baa92de0f1d002a93f8580609013a583c902f004b07a90098ae914c928b002c683a58248a58348b98c00489848b58c488a858448b98c002907a8b92f03bae0ec902248a58048a684a485207c01a58149088581209801a5814908858168aa6838e5808680c58018300cd007a5866a6a4c670285803868aa68958c688585a868998c00688583688582900668688a489848b58c2907c901f00cc905b008b98c00d0034cc2014c620120d402a9388582a200a582850fa920850fa0088a48b58caabdf202850fa920850f68aae888d0eca582850fa90a850fc682e8e8e050d0d220d40260a200bde902850fa920850fe8e009d0f2a90a850f602041424344454647482e707262716e6b0000505242514e4b98aa201c038580201c0349ff1869090a85820a0a65826580a88860ad11d0f0fbad10d0290f6002050304060305020001050309031014080c080008ebedf4f8080c1315f60aff01090bf7f5f5f7f6ec090b0a14ff

My suggestion is to press two times the "Trace less" button and then press the "Play" button.

Once the chessboard is displayed, enter your movement as D2D4 (no spaces, no Enter)
and the computer will start thinking.

It will be utterly SLOW!


Acknowledgements:

* Peter Ferrie for some pretty cool optimizations.


>> ATTENTION <<

Do you would like to learn to program 6502 assembler and
create Atari 2600 games? It is possible with my newest book
Programming Games for Atari 2600.

Now available from Lulu:

  Paperback
    https://www.lulu.com/shop/oscar-toledo-gutierrez/programming-games-for-atari-2600/paperback/product-pq9dg4.html

  Hardcover
    https://www.lulu.com/shop/oscar-toledo-gutierrez/programming-games-for-atari-2600/hardcover/product-n8z9r6.html

 eBook
    https://nanochess.org/store.html

These are some of the example programs documented profusely
in the book:

  * Game of Ball.
  * Wall Breaker.
  * Invaders.
  * The Lost Kingdom.
  * Diamond Craze.
