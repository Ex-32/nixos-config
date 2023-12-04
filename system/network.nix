{ config, pkgs, lib, nixpkgs, ... }:

{
  networking.networkmanager.enable = true;
  environment.persistence."/persist" = {
    directories = [
      "/etc/NetworkManager/system-connections"
    ];
  };

  # firewall config
  networking.firewall.enable = true;

  # port 57621 TCP/UDP is for spotify-sync
  # networking.firewall.allowedTCPPorts = [ 57621 ];
  # networking.firewall.allowedUDPPorts = [ 57621 ];
}
