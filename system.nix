# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, nixpkgs, ... }:

{

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # enable experimental flake support
  nix.settings.experimental-features = [ "nix-command" "flakes" ];
 
  hardware.opengl.enable = true;

  # bootloader
  #boot.loader.systemd-boot = {
  #  enable = true;
  #  memtest86.enable = true;
  #  consoleMode = "max";
  #};
  boot.loader.efi = {
      canTouchEfiVariables = true;
    efiSysMountPoint = "/boot/efi";
  };
  boot.loader.grub = {
      enable = true;
    device = "nodev";
      efiSupport = true;
      enableCryptodisk = true;
      font = "${pkgs.spleen}/share/fonts/misc/spleen-16x32.otf";
      fontSize = 32;
  };
  boot.tmp.useTmpfs = true;

  console = {
    earlySetup = true;
    packages = with pkgs; [ spleen ];
    font = "spleen-16x32";
#    colors = [
#      "11111b" # crust
#      "f38ba8" # red
#      "a6e3a1" # green
#      "fab387" # peach
#      "89b4fa" # blue
#      "cba6f7" # mauve
#      "89dceb" # sky
#      "a6adc8" # subtext 0
#      "1e1e2e" # base
#      "f5c2e7" # pink
#      "a6e3a1" # green (again...)
#      "f9e2af" # yellow
#      "74c7ec" # sapphire
#      "b3befe" # lavender
#      "94e2d5" # teal
#      "cdd6f4" # text
#    ];
  };

  systemd = {
    coredump.extraConfig = "Storage=none";
    user.services.polkit-agent = {
      description = "user polkit agent";
      wantedBy = [ "graphical-session.target" ];
      wants = [ "graphical-session.target" ];
      after = [ "graphical-session.target" ];
      serviceConfig = {
          Type = "simple";
          ExecStart = "${pkgs.mate.mate-polkit}/libexec/polkit-mate-authentication-agent-1";
          Restart = "on-failure";
          RestartSec = 1;
          RestartStopSec = 10;
      };
    };
  };

  networking.hostName = "nixbook"; 
  networking.networkmanager.enable = true;

  time.timeZone = "America/Chicago";
  i18n.defaultLocale = "en_US.UTF-8";
  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

  environment.variables = {
    EDITOR = "nvim";
    LESSHISTFILE = "-";
    MICRO_TRUECOLOR = "1";
    GHCUP_USE_XDG_DIRS = "1";
    _JAVA_AWT_WM_NONREPARENTING = "1";
  };

  environment.sessionVariables = {
    XDG_CACHE_HOME = "$HOME/.cache";
    XDG_CONFIG_HOME = "$HOME/.config";
    XDG_DATA_HOME = "$HOME/.local/share";
    XDG_STATE_HOME = "$HOME/.local/state";

    # XDG_BIN_HOME not officially part of the standard:
    XDG_BIN_HOME = "$HOME/.local/bin";

    XAUTHORITY = "$XDG_RUNTIME_DIR/Xauthority";
    xserverauthfile = "$XAUTHORITY";

    CARGO_HOME = "$XDG_DATA_HOME/cargo";
    RUSTUP_HOME = "$XDG_DATA_HOME/rustup";
    GOPATH = "$XDG_DATA_HOME/go";
    RBENV_ROOT = "$XDG_DATA_HOME/rbenv";
    GNUPGHOME = "$XDG_DATA_HOME/gnugp";
    WINEPREFIX = "$XDG_DATA_HOME/wine";
 
    PYTHONSTARTUP = "$XDG_CONFIG_HOME/python3/startup.py";
    GTK_RC_FILES = "$XDG_CONFIG_HOME/gtk-1.0/gtkrc";
    GTK_RC2_FILES = "$XDG_CONFIG_HOME/gtk-2.0/gtkrc";
    DOCKER_CONFIG = "$XDG_CONFIG_HOME/docker";
    _JAVA_OPTIONS = "-Djava.util.prefs.userRoot=$XDG_CONFIG_HOME/java";

    CUDA_CACHE_PATH = "$XDG_CACHE_HOME/nvidia/ComputeCache";

    PATH = "$HOME/.local/bin:$HOME/.local/share/cargo/bin:$HOME./local/share/go/bin";
  };

  # services.getty.extraArgs = [ "--noclear" ];

  # customize available shells
  programs.fish.enable = true;
  environment.shells = with pkgs; [
    fish
  ];

  programs.dconf.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.jenna = {
    isNormalUser = true;
    description = "Jenna Fligor";
    extraGroups = [ "networkmanager" "wheel" "video" ];
    shell = pkgs.fish;
  };

  environment.systemPackages = with pkgs; [
    bat
    btdu
    compsize
    curl
    file
    git
    gparted
    htop
    killall
    lsd
    mate.mate-polkit
    micro
    neofetch
    neovim
    scc
    tmux
    unzip
    wget
    wl-clipboard
    zip
  ];

  environment.shellAliases = {
    l = "lsd -lAh --no-symlink --date relative";
    ll = "lsd -lAh";
    ls = null;
    nix-fish = "nix-shell --command 'fish'";
    sc = "sudo systemctl";
    scu = "systemctl --user";
    jc = "journalctl";
    "..." = "cd ../..";
    "...." = "cd ../../..";
    "....." = "cd ../../../..";
    "......" = "cd ../../../../..";
    "......." = "cd ../../../../../..";
    "........" = "cd ../../../../../../..";
  };

  # enable CUPS to printing with avahi network support
  services.printing = {
    enable = true;
    startWhenNeeded = true;
  };
  services.avahi = {
      enable = true;
      nssmdns = true;
      openFirewall = true;
  };

  fonts.enableDefaultFonts = true;
  # fonts.enableDefaultPackages = true;

  services.fprintd.enable = true;
  security.pam.services.login.fprintAuth = false;
  security.pam.services.swaylock.text = ''
    # PAM configuration file for the swaylock screen locker. 
    # By default, it includes the 'login' configuration file 
    # (see /etc/pam.d/login)
    auth include login
  '';

  # Enable sound with pipewire.
  sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    wireplumber.enable = true;
    alsa.enable = true;
    #alsa.support32Bit = true;
    pulse.enable = true;
    jack.enable = true;
  };

  # Enable the OpenSSH daemon.
  services.openssh = {
      enable = false;
      ports = [ 3932 ];
      startWhenNeeded = true;
      settings.PermitRootLogin = "no";
  };

  # enable locate serivce w/ plocate
  services.locate = {
    enable = true;
    locate = pkgs.plocate;
    interval = "hourly";
    prunePaths = lib.mkOptionDefault [
      "/mnt/fsroot/"
      "/.snapshots/"
      "/home/.snapshots/"
    ];
    # stop warning about updatedb running as root (required for plocate)
    localuser = null;
  };

  # dbus service for disk control
  services.udisks2.enable = true;

  virtualisation.podman = {
    enable = true;
    dockerCompat = true;
  };

  # firewall config
  networking.firewall.enable = true;
  # port 57621 TCP/UDP is for spotify-sync
  networking.firewall.allowedTCPPorts = [ 57621 ];
  networking.firewall.allowedUDPPorts = [ 57621 ];

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?

}
