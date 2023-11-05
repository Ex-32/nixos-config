{ config, pkgs, lib, nixpkgs, ... }:

{
  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # enable experimental flake support
  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  nix.settings.use-xdg-base-directories = true;

  systemd.coredump.extraConfig = "Storage=none";

  environment.systemPackages = with pkgs; [
    btdu
    compsize
    curl
    file
    fzf
    git
    htop
    man-pages
    micro
    neofetch
    ripgrep
    tmux
    unzip
    wget
    zip
  ];

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?
}
