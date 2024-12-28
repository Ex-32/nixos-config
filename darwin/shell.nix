{
  config,
  pkgs,
  lib,
  nixpkgs,
  ...
}: {
  environment.variables = rec {
    # disable the less history file
    LESSHISTFILE = "-";

    # enable truecolor when using micro
    # TODO: use a wrapper around micro to avoid general env pollution
    MICRO_TRUECOLOR = "1";

    XDG_CONFIG_HOME = "$HOME/.config";
    XDG_CACHE_HOME = "$HOME/.cache";
    XDG_DATA_HOME = "$HOME/.local/share";

    CARGO_HOME = "${XDG_DATA_HOME}/cargo";
    RUSTUP_HOME = "${XDG_DATA_HOME}/rustup";
    GOPATH = "${XDG_DATA_HOME}/go";
    RBENV_ROOT = "${XDG_DATA_HOME}/rbenv";
    GNUPGHOME = "${XDG_DATA_HOME}/gnugp";
    WINEPREFIX = "${XDG_DATA_HOME}/wine";
    GRADLE_USER_HOME = "${XDG_DATA_HOME}/gradle";

    # NOTE: using _JAVA_OPTIONS to set the userRoot location and prevent the
    # creation of ~/.java can break poorly designed programs that hardcode
    # paths and so should *not* be set
    GTK_RC_FILES = "${XDG_CONFIG_HOME}/gtk-1.0/gtkrc";
    GTK_RC2_FILES = "${XDG_CONFIG_HOME}/gtk-2.0/gtkrc";
    NPM_CONFIG_USERCONFIG = "${XDG_CONFIG_HOME}/npm/npmrc";
    ZDOTDIR = "${XDG_CONFIG_HOME}/zsh";
  };

  environment.shellAliases = {
    ls = null;

    l = "${pkgs.lsd}/bin/lsd -lA --date relative --no-symlink";
    ll = "${pkgs.lsd}/bin/lsd -lA";

    drs = "darwin-rebuild switch --flake ~/.config/nix-darwin";

    # datetime = "${pkgs.coreutils}/bin/date '+%a %Y-%m-%d %H:%M:%S'";

    "..." = "cd ../..";
    "...." = "cd ../../..";
    "....." = "cd ../../../..";
    "......" = "cd ../../../../..";
    "......." = "cd ../../../../../..";
    "........" = "cd ../../../../../../..";
    "........." = "cd ../../../../../../../..";
    ".........." = "cd ../../../../../../../../..";
  };

  # misc shell utilities for interactive shell use
  environment.systemPackages = with pkgs; let
    python = python3.withPackages (p:
      with p; [
        numpy
        scipy
      ]);
  in [
    bat # a modern cat clone with line numbers and syntax highlighting
    du-dust # a modern du replacement designed for interactive use
    duf # a modern df replacement with tailored for human readability
    fselect # and SQL inspired find utility for querying the filesystem
    fzf # fuzzy search the filesystem for files/directories
    htop # the best way to monitor processes this side of the solar system
    ripgrep # grep the filesystem crazy fast
    trash-cli # fuck i didn't mean to delete that...
    zellij # tmux but dramatic
    python # python with dependencies
    python.pkgs.ptpython # better python REPL
  ];
}
