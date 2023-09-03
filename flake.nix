{
  description = "savau's flakey xmonad configuration";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs;
    home-manager = {
      url = github:nix-community/home-manager;
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home-manager, ... }: let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
  in {
    homeConfigurations."savau" = home-manager.lib.homeManagerConfiguration {
      inherit pkgs;

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
        config = builtins.readFile ./xmonad-monolith.hs; # TODO include modules from lib as build inputs and remove then-obsolete xmonad-monolith.hs
      };
    };
  };
}
