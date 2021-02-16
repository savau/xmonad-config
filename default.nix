{ config, pkgs, ... }:

# requirements for this XMonad configuration
{
  hardware = {
    bluetooth.enable = true;
    pulseaudio.enable = true;
  };
  
  networking.networkmanager.enable = true;

  services.blueman.enable = true;

  environment.systemPackages = with pkgs; [
    # screensaver and locker
    xscreensaver

    # status bar
    xmobar

    # system tray
    stalonetray

    # system tray stuff
    birdtray
    volumeicon

    # dynamic menu
    dmenu

    #autoconf automake pkg-config  # needed for xmonad integration with xfce4-panel

    # frequently used applications and startup applications
    firefox
    jetbrains.idea-community
    keepassxc
    megasync
    octave
    signal-desktop
    texstudio
    thunar
    thunderbird
    zulip
  ];
}
