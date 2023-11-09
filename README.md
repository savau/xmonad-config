# xmonad-config
My personal XMonad configuration

## Installation

### On NixOS with home-manager

The `xmonad.hs` can be deployed using [home-manager](https://github.com/nix-community/home-manager).

To install, put the following into your `home.nix`:
```nix
{
  xsession = {
    enable = true;
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      extraPackages = haskellPackages: with haskellPackages; [
        xmonad xmonad-contrib xmonad-extras
        dbus
        gtk-sni-tray
        status-notifier-item
        tuple
      ];
      config = ./path/to/your/xmonad.hs;
    };
  };
}
```

With the above-mentioned nix configuration, xmonad will be built with your configuration on every `nixos-rebuild`.

**Note:** I did not yet find out how to install modular xmonad configurations, i.e. `xmonad.hs` files depending on modules defined in `./lib/`, using home-manager. If there is a way to do so, the polylithic (main) version of this configuration (i.e. `xmonad.hs`) may also be used in home-manager.

### On NixOS without home-manager

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

### On Arch
Assuming a display manager such as LightDM is already installed and configured:

1. Install required packages:  
  ```sudo pacman -S xmonad xmonad-contrib xmonad-utils xmobar xscreensaver stalonetray```
2. Clone this repository into `~/.xmonad/`
3. Recompile and restart XMonad:  
  ```xmonad --recompile && xmonad --restart```
