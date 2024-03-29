{
  config,
  pkgs,
  lib,
  nixpkgs,
  ...
}: {
  # NetworkManager: networking for noobs since 2004
  networking.networkmanager.enable = true;
  networking.firewall.enable = true;

  # port 57621 TCP/UDP is for spotify-sync
  # networking.firewall.allowedTCPPorts = [ 57621 ];
  # networking.firewall.allowedUDPPorts = [ 57621 ];
}
