# Rallisp

An attempt at a turn-based racing game.

## Controls

To enter your acceleration/braking and steering, click on the bottom-right corner.

## Credits and license

Based on the [Hoot Game Jam
Template](https://codeberg.org/spritely/hoot-game-jam-template). The
license is the same Apache License.

## Building

The fastest way to get everything you need is to use [GNU
Guix](https://guix.gnu.org), a wonderful package manager written in
Scheme.

Once you have Guix, the development environment with all necessary
dependencies can be created:

```
guix shell
```

To build the game, run:

```
make
```

To launch a development web server, run:

```
make serve
```

To check if the program works, visit https://localhost:8088 in your
web browser.  We recommend using Mozilla Firefox or Google Chrome.
Hoot is not supported on Safari at this time.

When it's time to publish the game to itch.io, run:

```
make bundle
```

Upload the resulting zip file to your itch.io game page and share your
game with others!  Have fun!

