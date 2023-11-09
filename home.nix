{ lib, pkgs, home, ... }: {

  home.packages = with nixpkgs; [
    dmenu
  ];

  xsession.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    extraPackages = haskellPackages: with haskellPackages; [
      xmonad xmonad-contrib xmonad-extras
      dbus
      gtk-sni-tray
      status-notifier-item
      tuple
    ];
    config = builtins.readFile ./xmonad.hs;
  };

}
