{
  config,
  lib,
  pkgs,
  ...
}: {
  # Simply install just the packages
  environment.packages = with pkgs; [
    # editors
    micro
    neovim

    # basic utilities
    procps
    diffutils
    findutils
    utillinux
    tzdata
    hostname
    gnugrep
    gnused
    gnutar
    bzip2
    gzip
    xz
    zip
    unzip

    # docs
    man
    linux-manual

    # dev utils
    git
    gh

    # networking utilities
    openssh
    nmap
    iproute2

    # shell utilities
    fish
    dust
    duf
  ];

  user.shell = lib.getExe pkgs.fish;

  # Backup etc files instead of failing to activate generation if a file already exists in /etc
  environment.etcBackupExtension = ".bak";

  # Set up nix for flakes
  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';

  system.stateVersion = "24.05";
}
