{ config, pkgs, lib, nixpkgs, inputs, ... }:

{
  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  nix.settings = {
    # enable experimental flake support
    experimental-features = [ "nix-command" "flakes" ];
    # xdg my beloved
    use-xdg-base-directories = true;
    # nix for the big cheese only
    allowed-users = lib.mkForce [ "@wheel" ];
  };

  systemd.coredump.extraConfig = "Storage=none";
  security.sudo.execWheelOnly = true;

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
