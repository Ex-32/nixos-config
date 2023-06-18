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
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.tmp.useTmpfs = true;
  # boot.initrd.secrets = {
    # "/crypto_keyfile.bin" = null;
  # };

  # networking
  networking.hostName = "nixos-pc"; 
  networking.networkmanager = {
    enable = true;
    wifi.backend = "iwd";
  };

  # Set your time zone.
  time.timeZone = "America/Chicago";

  # Select internationalisation properties.
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

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  # Enable the KDE Plasma Desktop Environment, using startx instead of a DM;
  services.xserver.displayManager.startx.enable = true;
  services.xserver.desktopManager.plasma5.enable = true;
  # Remove default apps from KDE Plasma
  environment.plasma5.excludePackages = with pkgs.libsForQt5; [
    gwenview
    okular
    khelpcenter
    konsole
  ];

  # customize avalible shells
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

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    bat
    btdu
    catppuccin-cursors
    catppuccin-gtk
    catppuccin-kde
    catppuccin-kvantum
    catppuccin-papirus-folders
    compsize
    curl
    file
    git
    gparted
    htop
    libsForQt5.bismuth
    libsForQt5.qtstyleplugin-kvantum
    lsd
    micro
    neofetch
    neovim
    scc
    tmux
    unzip
    wget
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

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?

}
