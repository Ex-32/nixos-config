{ config, pkgs, lib, nixpkgs, inputs, ... }:

{
  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # enable experimental flake support
  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  nix.settings.use-xdg-base-directories = true;

  systemd.coredump.extraConfig = "Storage=none";
  security.sudo.execWheelOnly = true;

  environment.persistence."/persist" = {
    directories = [
      "/etc/nixos"
      "/var/lib/nixos"
      "/var/log"
      # "/var/lib/bluetooth" # add this to system/bluetooth.nix if/when it exists
    ];
    files = [
      "/etc/machine-id"
    ];
  };

  environment.systemPackages = with pkgs; [
    btdu
    compsize
    curl
    file
    git
    git-crypt
    lsof
    man-pages
    micro
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
  system.stateVersion = "23.11"; # Did you read the comment?
}
