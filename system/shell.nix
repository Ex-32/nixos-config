{ config, pkgs, lib, nixpkgs, ... }:

{
  programs.fish = {
    enable = true;
    useBabelfish = true;
  };

  environment.shells = with pkgs; [
    fish
  ];

  environment.variables = {
    EDITOR = "nvim";
    LESSHISTFILE = "-";
    MICRO_TRUECOLOR = "1";
    _JAVA_AWT_WM_NONREPARENTING = "1";
    FZF_DEFAULT_OPTS = builtins.concatStringsSep " " [
      "--color=bg+:#313244,bg:#1e1e2e,spinner:#f5e0dc,hl:#f38ba8"
      "--color=fg:#cdd6f4,header:#f38ba8,info:#cba6f7,pointer:#f5e0dc"
      "--color=marker:#f5e0dc,fg+:#cdd6f4,prompt:#cba6f7,hl+:#f38ba8"
    ];
  };

  environment.sessionVariables = {
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
 
    PYTHONSTARTUP = "$XDG_CONFIG_HOME/python3/startup.py";
    GTK_RC_FILES = "$XDG_CONFIG_HOME/gtk-1.0/gtkrc";
    GTK_RC2_FILES = "$XDG_CONFIG_HOME/gtk-2.0/gtkrc";
    # DOCKER_CONFIG = "$XDG_CONFIG_HOME/docker"; # this can cause some issues with distrobox + podman
    # _JAVA_OPTIONS = "-Djava.util.prefs.userRoot=$XDG_CONFIG_HOME/java"; # conceptually good, but causes fuckery
    NPM_CONFIG_USERCONFIG = "$XDG_CONFIG_HOME/npm/npmrc";

    CUDA_CACHE_PATH = "$XDG_CACHE_HOME/nvidia/ComputeCache";

    PATH = [
      "$HOME/.local/share/cargo/bin"
      "$HOME/.local/share/go/bin"
    ];
  };

  environment.shellAliases = {
    l = "lsd -lAh --no-symlink --date relative";
    ll = "lsd -lAh";
    ls = null;
    sc = "sudo systemctl";
    scu = "systemctl --user";
    jc = "journalctl";
    "..." = "cd ../..";
    "...." = "cd ../../..";
    "....." = "cd ../../../..";
    "......" = "cd ../../../../..";
    "......." = "cd ../../../../../..";
    "........" = "cd ../../../../../../..";
    "........." = "cd ../../../../../../../..";
    ".........." = "cd ../../../../../../../../..";
    nrs = "sudo nixos-rebuild switch";
    nrb = "sudo nixos-rebuild boot";
  };

  environment.systemPackages = with pkgs; [
    bat
    du-dust
    fzf
    htop
    lsd
    neofetch
    pridecat
    ripgrep
    tmux
    trash-cli
  ];
}
