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

  home.packages = with pkgs; [
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
    catppuccin-kde
    catppuccin-kvantum
    clang
    discord
    dconf # needed by home-manager gtk config
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

  programs.gh = {
    enable = true;
    enableGitCredentialHelper = true;
  };
  programs.git = {
    enable = true;
    userName = "Ex-32";
    userEmail = "jenna@fligor.net";
    extraConfig = {
      init.defaultBranch = "main";
      safe.directory = "/etc/nixos";
    };
  };

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

  gtk = {
    enable = true;
    theme = {
      package = pkgs.catppuccin-gtk;
      name = "Catppuccin-Mocha-Standard-Mauve-Dark";
    };
    font = {
      package = pkgs.raleway;
      name = "Raleway";
      size = 10;
    };
    iconTheme = {
      package = pkgs.catppuccin-papirus-folders;
      name = "Papirus-Dark";
    };
    cursorTheme = {
      package = pkgs.catppuccin-cursors;
      name = "Catppuccin-Mocha-Mauve-Cursors";
      size = 48;
    };
    gtk2.configLocation = "${config.xdg.configHome}/gtk-2.0/gtkrc";
  };

  nixpkgs.overlays = [
    (final: prev: {
      catppuccin-papirus-folders = prev.catppuccin-papirus-folders.override {
        flavor = "mocha";
        accent = "mauve";    
      };
      catppuccin-kvantum = prev.catppuccin-kvantum.override {
        variant = "Mocha";
        accent  = "Mauve";    
      };
      catppuccin-gtk = prev.catppuccin-gtk.override {
        accents = [ "mauve" ];
        variant = "mocha";
      };
      catppuccin-kde = prev.catppuccin-kde.override {
        flavour = [ "mocha" ];
        accents = [ "mauve" ];
      };
      nerdfonts = prev.nerdfonts.override {
        fonts = [ "FiraCode" ];
      };
    })
  ];

  programs.home-manager.enable = true;
  home.stateVersion = "23.05";
}
