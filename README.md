Bare Metal demos written in Ada
===============================

Important notice
----------------

This repository uses submodules. To clone it, you need to use the --recursive git option:

    $ git clone --recursive https://github.com/lambourg/Ada_Bare_Metal_Demos.git

You will also need run-time support for your board. More instruction can be
found in the Ada Drivers Library submodule (the drivers folder). You can also
see directly the installation instructions at https://github.com/AdaCore/embedded-runtimes.git

In order to build and flash those demos on your specific target, you will need
an Ada run-time supporting your board. You can also see directly
https://github.com/AdaCore/embedded-runtimes.git to build and install various
run-times targeting ARM boards.

Content:
--------

The following demos/examples are available:

  * **2048**: A demo of the famous game, with automatic solver, for STM32 discovery boards
  * **balls**: Simple bouncing balls, for STM32 discovery boards
  * **conway**: A version of the "Game of Life", for STM32 discovery boards
  * **fractals**: Mandelbrot/Julia fractals calculator, with zooming capability using the touch screen, for STM32 discovery boards
  * **rpi2-mmc**: SD/MMC card demo for the Raspberry Pi2/Pi3
  * **rpi2-wolf**: A raycaster demo "a la" Castle Wolfenstein. For Raspberry Pi2/3.
  * **sdcard**: SDCard demo for the STM32 discovery boards
  * **wav_player**: A simple WAV player to play songs from sdcards. For the STM32Disco boards with display and audio support.
  * **wolf**: A raycaster demo "a la" Castle Wolfenstein. For STM32disco boards.
  * **zynq-eth**: Ethernet experimentations for the Zynq7k.

This repository also contains a FAT FS Library that is used by some of the
demos. This library is present in the 'services' folder.
