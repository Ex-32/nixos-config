{ config, pkgs, lib, nixpkgs, ... }:

{
  virtualisation.podman = {
    enable = true;
    dockerCompat = true;
  };
  environment.systemPackages = with pkgs; [ distrobox ];
}
