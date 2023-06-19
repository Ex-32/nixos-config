# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, nixpkgs, ... }:

{

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # configure home-manager
  home-manager.useUserPackages = true;
  home-manager.useGlobalPkgs = true;

  # enable experimental flake support
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # needed for some 32-bit games
  hardware.opengl.driSupport32Bit = true;

  # bootloader
  boot.loader.systemd-boot = {
    enable = true;
    memtest86.enable = true;
    consoleMode = "max";
  };
  boot.loader.efi.canTouchEfiVariables = true;
  boot.tmp.useTmpfs = true;

  console = {
    earlySetup = true;
    packages = with pkgs; [ spleen ];
    font = "spleen-16x32";
    colors = [
      "11111b" # crust
      "f38ba8" # red
      "a6e3a1" # green
      "fab387" # peach
      "89b4fa" # blue
      "cba6f7" # mauve
      "89dceb" # sky
      "a6adc8" # subtext 0
      "1e1e2e" # base
      "f5c2e7" # pink
      "a6e3a1" # green (again...)
      "f9e2af" # yellow
      "74c7ec" # sapphire
      "b3befe" # lavender
      "94e2d5" # teal
      "cdd6f4" # text
    ];
  };

  systemd.coredump.extraConfig = "Storage=none";

  # networking
  networking.hostName = "nixos-pc"; 
  networking.networkmanager = {
    enable = true;
    wifi.backend = "iwd";
  };

  # Set your time zone.
  time.timeZone = "America/Chicago";

  # Select internationalization properties.
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

  services.getty.extraArgs = [ "--noclear" ];

  services.xserver = {
    enable = true;
    displayManager.startx.enable = true;
    desktopManager.plasma5.enable = true;
    excludePackages = with pkgs; [
      xterm
    ];
  };
  environment.plasma5.excludePackages = with pkgs.libsForQt5; [
    elisa
    gwenview
    khelpcenter
    konsole
    okular
  ];

  # customize available shells
  programs.fish.enable = true;
  environment.shells = with pkgs; [
    fish
  ];

  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true;
    dedicatedServer.openFirewall = true;
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.user = {
    isNormalUser = true;
    description = "NixOS User";
    extraGroups = [ "networkmanager" "wheel" ];
    shell = pkgs.fish;
  };
  security.pam.services.user.enableKwallet = true;

  environment.systemPackages = with pkgs; [
    bat
    btdu
    compsize
    curl
    file
    git
    gparted
    htop
    lsd
    micro
    neofetch
    neovim
    scc
    tmux
    unzip
    wget
    xclip
    zip
  ];

  environment.shellAliases = {
    l = "lsd -lAh --date relative";
    ll = "lsd -lAh";
    ls = null;
  };

  # Configure keymap in X11
  services.xserver = {
    layout = "us";
    xkbVariant = "";
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

  # Enable sound with pipewire.
  sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    wireplumber.enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    #jack.enable = true;
  };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Enable the OpenSSH daemon.
  services.openssh = {
  	enable = true;
  	ports = [ 3932 ];
  	startWhenNeeded = true;
  	settings.PermitRootLogin = "no";
  };

  # enable locate serivce w/ plocate
  services.locate = {
    enable = true;
    locate = pkgs.plocate;
    interval = "hourly";
    # stop warning about updatedb running as root (required for plocate)
    localuser = null;
  };

  # Open ports in the firewall.
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
