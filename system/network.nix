{ config, pkgs, lib, nixpkgs, ... }:

{
  networking.hostName = "nixbook"; 
  networking.networkmanager.enable = true;

  # firewall config
  networking.firewall.enable = true;

  # port 57621 TCP/UDP is for spotify-sync
  # networking.firewall.allowedTCPPorts = [ 57621 ];
  # networking.firewall.allowedUDPPorts = [ 57621 ];
}
