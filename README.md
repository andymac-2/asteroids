# Asteroids
(C) Andrew Pritchard 2018 Some BSD license I dunno.

![A screenshot Yay!](docs/screenshot.png)

To play: arrow keys to rotate, and move. `space` to fire. `P` to pause. `Q` to quit.

## Compile from source

You will need the Haskell toot stack to compile this program from source. Instructions for installation can be found [here.](https://docs.haskellstack.org/en/stable/README/)

After stack is installed, you will also need to install `sdl2`, `sdl2_ttf` and `sdl2_image`.  On a mac, this can be done with Homebrew:

```bash
brew install sdl2 sdl2_ttf sdl2_image
```

Compilation can the proceed:

```bash
stack build && stack exec asteroids-exe
```

