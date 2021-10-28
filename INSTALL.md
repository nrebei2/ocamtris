# Installation

## Windows with WSL + Ubuntu

To run ocamtris, the [Graphics](https://ocaml.org/releases/4.02/htmlman/libref/Graphics.html) module must be installed:

In WSL terminal, run commands
1. `sudo apt install pkg-config`
2. `opam install graphics`

In order for ocamtris to display the screen, a X11 display server is needed. For this project, we used Xming:

1. Install the [Xming X Server for Windows](https://sourceforge.net/projects/xming/)
2. Start XLaunch and use settings:
    - Leave **Multiple windows** checked in Display settings (default)
    - Leave **Start no client** checked in Session type (default)
    - Check **Clipboard** in Additional paramaters (defualt)
        - If using WSL 2, additionally check **No Access Control** 
3. In WSL terminal, run
    - If on WSL: `export DISPLAY=:0`
    - if on WSL 2: `export DISPLAY=$(route.exe print | grep 0.0.0.0 | head -1 | awk '{print $4}'):0.0`


## Mac

1. To run ocamtris, the [Graphics](https://ocaml.org/releases/4.02/htmlman/libref/Graphics.html) module must be installed
2. In terminal, run the command `opam install graphics`
3. In order for ocamtris to display the screen, XQuartz is also needed, which you can install from [https://www.xquartz.org/]
4. To run the command below (`make play`), you may need to have XQuartz running in the background, or run the command from the XQuartz terminal, which you can open by opening XQuartz and selecting Applications > Terminal from the menu bar.



## Run Ocamtris

1. Follow the above instructions given your specific operating system
2. In terminal, change your working directory to ocamtris and run `make play` to build and launch the game.
3. Have fun!