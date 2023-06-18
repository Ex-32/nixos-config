{ config, pkgs, lib, ... }:
let
  flake-compat = builtins.fetchTarball {
    url = "https://github.com/edolstra/flake-compat/archive/master.tar.gz";
    sha256 = "1prd9b1xx8c0sfwnyzkspplh30m613j42l1k789s521f4kv4c2z2";
  };
  spicetify-nix =
    (import flake-compat {
      src = builtins.fetchTarball {
        url = "https://github.com/the-argus/spicetify-nix/archive/master.zip";
        sha256 = "0l9rf5a80qq05kpkp1pi56saa0kmlps94amvwx4fzpjvikgyb339";
      };
    }).defaultNix;
  spicePkgs = spicetify-nix.packages.${pkgs.system}.default;
in
{
  imports = [ spicetify-nix.homeManagerModule ];

  packages = with pkgs; [
    bacon
    btop
    cargo
    cargo-audit
    cargo-cache
    cargo-edit
    cargo-flamegraph
    cargo-generate
    cargo-update
    cargo-watch
    catppuccin-cursors
    catppuccin-gtk
    catppuccin-kde
    catppuccin-kvantum
    catppuccin-papirus-folders
    clang
    discord
    gh
    gimp-with-plugins
    inkscape-with-extensions
    kitty
    libsForQt5.bismuth
    libsForQt5.qtstyleplugin-kvantum
    lutris
    nerdfonts
    nomacs
    obs-studio
    onlyoffice-bin
    rofi
    starship
    tdrop
    vivaldi
    zoxide
  ];

  programs.spicetify = {
    enable = true;
    theme = spicePkgs.themes.catppuccin-mocha;
    enabledExtensions = with spicePkgs.extensions; [
      autoSkipVideo
      autoSkipExplicit
      shuffle
      hidePodcasts
    ];
  };

  programs.home-manager.enable = true;
  home.stateVersion = "23.05";
}
