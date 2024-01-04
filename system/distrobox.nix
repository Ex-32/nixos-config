{ config, pkgs, lib, nixpkgs, ... }:

{
  # this enables podman with docker compatibility as a backend for distrobox,
  # podman is used over docker because it supports non-root containers; since
  # the containers are are used interactively and have broad filesystem access,
  # running the container as root is inadvisable for the same reason that
  # logging in as root, or running a graphical environment as root is
  # inadvisable
  virtualisation.podman = {
    enable = true;
    dockerCompat = true;
  };
  environment.systemPackages = with pkgs; [ distrobox ];
}
