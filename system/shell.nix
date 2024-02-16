{ config, pkgs, lib, nixpkgs, ... }:

{
  programs.fish = {
    enable = true;
    # babelfish is a modern, more performant replacement for foreign-env which
    # allows capturing environment variable changes from non-fish shells and
    # propagating them, this setting uses babelfish to load the environment
    # from files like /etc/profile
    useBabelfish = true;
  };

  environment.shells = with pkgs; [
    fish
  ];

  environment.variables = {
    # disable the less history file
    LESSHISTFILE = "-";

    # enable truecolor when using micro
    # TODO: use a wrapper around micro to avoid general env pollution
    MICRO_TRUECOLOR = "1";

    # this is a hacky but unavoidable fix to get many graphical applications
    # written in java to work on wayland and some more exotic X window managers
    _JAVA_AWT_WM_NONREPARENTING = "1";

    # sets fzf's theme to catppuccin
    # TODO: use a wrapper around fzf to avoid general env pollution
    FZF_DEFAULT_OPTS = builtins.concatStringsSep " " [
      "--color=bg+:#313244,bg:#1e1e2e,spinner:#f5e0dc,hl:#f38ba8"
      "--color=fg:#cdd6f4,header:#f38ba8,info:#cba6f7,pointer:#f5e0dc"
      "--color=marker:#f5e0dc,fg+:#cdd6f4,prompt:#cba6f7,hl+:#f38ba8"
    ];
  };

  environment.sessionVariables = {
    # these are the default xdg locations, but some improperly designed
    # programs only use xdg locations if they're explicitly defined, and it
    # provides an easy way to reference them in other variable declarations
    XDG_CACHE_HOME = "$HOME/.cache";
    XDG_CONFIG_HOME = "$HOME/.config";
    XDG_DATA_HOME = "$HOME/.local/share";
    XDG_STATE_HOME = "$HOME/.local/state";

    XAUTHORITY = "$XDG_RUNTIME_DIR/Xauthority";

    CARGO_HOME = "$XDG_DATA_HOME/cargo";
    RUSTUP_HOME = "$XDG_DATA_HOME/rustup";
    GOPATH = "$XDG_DATA_HOME/go";
    RBENV_ROOT = "$XDG_DATA_HOME/rbenv";
    GNUPGHOME = "$XDG_DATA_HOME/gnugp";
    WINEPREFIX = "$XDG_DATA_HOME/wine";
    GRADLE_USER_HOME = "$XDG_DATA_HOME/gradle";

    # NOTE: using _JAVA_OPTIONS to set the userRoot location and prevent the
    # creation of ~/.java can break poorly designed programs with hardcoded
    # paths and so should *not* be set
    PYTHONSTARTUP = "$XDG_CONFIG_HOME/python3/startup.py";
    GTK_RC_FILES = "$XDG_CONFIG_HOME/gtk-1.0/gtkrc";
    GTK_RC2_FILES = "$XDG_CONFIG_HOME/gtk-2.0/gtkrc";
    NPM_CONFIG_USERCONFIG = "$XDG_CONFIG_HOME/npm/npmrc";

    # this sets the location for the cuda compute cache
    # TODO: only enable this environment variable on nvidia systems
    CUDA_CACHE_PATH = "$XDG_CACHE_HOME/nvidia/ComputeCache";

    # programs can be installed from source code using cargo for crates.io and
    # go for pkgs.go.dev, these packages get installed into their own
    # directories that must be added to the PATH
    PATH = [
      "$HOME/.local/share/cargo/bin"
      "$HOME/.local/share/go/bin"
    ];
  };

  environment.shellAliases = {

    l = "lsd -lAh --no-symlink --date relative";
    ll = "lsd -lAh";
    ls = null; # this disables nixos's default alias for ls

    sc = "sudo systemctl";
    scu = "systemctl --user";
    jc = "journalctl";

    nrs = "sudo nixos-rebuild switch";
    nrb = "sudo nixos-rebuild boot";

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
  environment.systemPackages = with pkgs; [
    bat       # a modern cat clone with line numbers and syntax highlighting
    du-dust   # a modern du replacement designed for interactive use
    fzf       # fuzzy search the filesystem for files/directories
    htop      # the best way to monitor processes this side of the solar system
    lsd       # a modern ls clone with features like icons and relative time
    neofetch  # show off in style
    pridecat  # a silly cat clone with a sense of pride
    ripgrep   # grep the filesystem crazy fast
    tmux      # terminals all the way down
    trash-cli # fuck i didn't mean to delete that...
  ];
}
