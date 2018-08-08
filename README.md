# libinput.el/touchgrid.el

This Emacs library divides the screen into zones and executed actions
when those zones are touched.  This is done by parsing the output from
the libinput tools.

For a video demostration, see this
https://lars.ingebrigtsen.no/2018/08/07/innovations-in-emacs-touch-interfacing/
blog post.

## Getting Started

This isn't a turnkey solution package: It's more of a framework that
has a bunch of example actions (i.e., the ones I'm using to control my
specific touch-screen/Emacs video browser/mpv player).  To use this,
you have to understand Emacs Lisp and hack a bit, but the general
framework should be sound.  Soundish.

It should work under both Wayland and X, but you need a rather recent
Emacs.

### Prerequisites

On Ubuntu, your user needs to be in the input group to be able to
access the events.  

```
$ sudo adduser `whoami` input
```

We also need programs to parse the inputs and do various actions:

```
$ sudo apt install libinput-tools xdotool onboard qiv
```

The main idea is to define a grid (how detailed is you to you) and
fill in the actions you want to have happen.  Here's a grid of actions
for a video player:


```
(backward-1m backward-10s pause forward-10s forward-1m)
(backward-1m backward-10s pause forward-10s forward-1m)
(none        none         quit  none        none      ) 
(grid        none         pause none        none      )
(keyboard    none         pause none        show-progress)
```

When you tap on the "pause" box, the touchgrid--pause function will be
called, which should then do whatever's needed to pause the video
player.  A likely command would be to use xdotool to focus the player
and then send it a SPC command, or whatever the video player uses.

The touchgrid.el file has a bunch of commands that I use for an
Emacs-based video browser that then calls mpv and communicates over
the mpv command socket.  See https://github.com/larsmagne/movie.el for
details.

