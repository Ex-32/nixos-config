{ config, pkgs, lib, nixpkgs, ... }:

{

  # vial udev rule
  services.udev.extraRules = ''
    KERNEL=="hidraw*", SUBSYSTEM=="hidraw", ATTRS{serial}=="*vial:f64c2b3c*", MODE="0660", GROUP="users", TAG+="uaccess", TAG+="udev-acl"
  '';

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # enable experimental flake support
  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  nix.settings.use-xdg-base-directories = true;

  hardware.opengl.enable = true;

  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true; # Open ports in the firewall for Steam Remote Play
    dedicatedServer.openFirewall = true; # Open ports in the firewall for Source Dedicated Server
  };

  boot.tmp.useTmpfs = true;

  systemd.coredump.extraConfig = "Storage=none";

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

  environment.systemPackages = with pkgs; [
    btdu
    compsize
    curl
    file
    git
    gparted
    htop
    killall
    man-pages
    micro
    neofetch
    neovim
    ripgrep
    scc
    tmux
    unzip
    wget
    zip
  ];

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

  fonts.enableDefaultPackages = true;

  services.fprintd.enable = true;
  security.pam.services.login.fprintAuth = false;
  security.pam.services.swaylock.fprintAuth = false;

  programs.dconf.enable = true;

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
    package = pkgs.plocate;
    interval = "hourly";
    prunePaths = lib.mkOptionDefault [
      "/mnt/fsroot/"
      "/home/.snapshots/"
    ];
    # stop warning about updatedb running as root (required for plocate)
    localuser = null;
  };

  # dbus service for disk control
  services.udisks2.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?

}
