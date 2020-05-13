# xmonad-config
My personal XMonad setup

## Installation

On Arch (assuming a display manager such as LightDM is already configured):

1. Install required packages:
  ```sudo pacman -S xmonad xmonad-contrib xmonad-utils xmobar xscreensaver stalonetray```
2. Clone this repository into `~/.xmonad/`
3. Recompile and restart XMonad (it might be necessary to kill and restart `stalonetray` as well):
  ```xmonad --recompile; xmonad --restart```
