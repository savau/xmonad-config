# xmonad-config
My personal XMonad configuration

## Installation

On NixOS:

1. Clone this repository into `~/.xmonad/`
2. Enable XMonad in your NixOS configuration:
```
{ config, pkgs, callPackage, ... }:

{
  services.xserver = {
    windowManager = {
      xmonad = {
        enable = true;
        enableContribAndExtras = true;
        extraPackages = haskellPackages : [
          haskellPackages.xmonad-contrib
          haskellPackages.xmonad-extras
          haskellPackages.xmonad
        ];
      };
    };
    # set as default, e.g. in combination with your prefered desktopManager or as standalone:
    displayManager.defaultSession = "none+xmonad";
  };
}
```
3. `nixos-rebuild switch`

On Arch (assuming a display manager such as LightDM is already installed and configured):

1. Install required packages:  
  ```sudo pacman -S xmonad xmonad-contrib xmonad-utils xmobar xscreensaver stalonetray```
2. Clone this repository into `~/.xmonad/`
3. Recompile and restart XMonad:  
  ```xmonad --recompile && xmonad --restart```
