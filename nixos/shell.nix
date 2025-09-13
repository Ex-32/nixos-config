{
  config,
  pkgs,
  lib,
  nixpkgs,
  ...
}: let
  fromSet = set: list: builtins.map (name: builtins.getAttr name set) list;
  pypkgs = [
    "jupyter"
    "numpy"
    "opencv4"
    "plotille"
    "pwntools"
    "scipy"
    "sympy"
  ];
in {
  programs.zsh = {
    enable = true;
    enableCompletion = true;
    enableBashCompletion = true;
    autosuggestions.enable = true;
  };

  programs.xonsh = {
    enable = true;
    package = pkgs.xonsh;
    extraPackages = ps:
      [
        pkgs.xonsh.xontribs.xonsh-direnv
      ]
      ++ (fromSet ps pypkgs);
  };

  environment.variables = rec {
    # disable the less history file
    LESSHISTFILE = "-";

    # this is a hacky but unavoidable fix to get many graphical applications
    # written in java to work on wayland and some more exotic X window managers
    _JAVA_AWT_WM_NONREPARENTING = "1";

    # this tells electron applications to use a native wayland backend if
    # available, as best i can tell it has no adverse effects on X11 so i've
    # just set it to always be enabled
    NIXOS_OZONE_WL = "1";

    XDG_CONFIG_HOME = "$HOME/.config";
    XDG_CACHE_HOME = "$HOME/.cache";
    XDG_DATA_HOME = "$HOME/.local/share";
    XDG_DATA_DIRS = let
      gsettings = pkgs.gsettings-desktop-schemas;
    in [
      "${gsettings}/share/gsettings-schemas/${gsettings.name}"
      "${pkgs.gtk3}/share/gsettings-schemas/${pkgs.gtk3.name}"
    ];

    XAUTHORITY = "$XDG_RUNTIME_DIR/Xauthority";

    CARGO_HOME = "${XDG_DATA_HOME}/cargo";
    GNUPGHOME = "${XDG_DATA_HOME}/gnugp";
    GOPATH = "${XDG_DATA_HOME}/go";
    GRADLE_USER_HOME = "${XDG_DATA_HOME}/gradle";
    RBENV_ROOT = "${XDG_DATA_HOME}/rbenv";
    RUSTUP_HOME = "${XDG_DATA_HOME}/rustup";
    WINEPREFIX = "${XDG_DATA_HOME}/wine";

    # NOTE: using _JAVA_OPTIONS to set the userRoot location and prevent the
    # creation of ~/.java can break poorly designed programs that hardcode
    # paths and so should *not* be set
    GTK_RC_FILES = "${XDG_CONFIG_HOME}/gtk-1.0/gtkrc";
    GTK_RC2_FILES = "${XDG_CONFIG_HOME}/gtk-2.0/gtkrc";
    NPM_CONFIG_USERCONFIG = "${XDG_CONFIG_HOME}/npm/npmrc";

    # this sets the location for the cuda compute cache
    # TODO: only enable this environment variable on nvidia systems
    CUDA_CACHE_PATH = "${XDG_CACHE_HOME}/nvidia/ComputeCache";
  };

  environment.shellAliases = {
    ls = null;

    l = "${lib.getExe pkgs.lsd} -lA --date relative --no-symlink";
    ll = "${lib.getExe pkgs.lsd} -lA";

    sc = "doas systemctl";
    scu = "systemctl --user";
    jc = "journalctl";

    nor = "nh os switch -a /etc/nixos";

    datetime = "${pkgs.coreutils}/bin/date '+%a %Y-%m-%d %H:%M:%S'";

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
  environment.systemPackages =
    (with pkgs; [
      bat # a modern cat clone with line numbers and syntax highlighting
      du-dust # a modern du replacement designed for interactive use
      duf # a modern df replacement with tailored for human readability
      fselect # and SQL inspired find utility for querying the filesystem
      fzf # fuzzy search the filesystem for files/directories
      htop # the best way to monitor processes this side of the solar system
      lsd # modernized ls rewrite (better version of e{x,z}a imo)
      ripgrep # grep the filesystem crazy fast
      trash-cli # fuck i didn't mean to delete that...
      zellij # tmux but dramatic
      hyperfine
    ])
    ++ [(pkgs.python3.withPackages (ps: fromSet ps pypkgs))];
}
