{ config, pkgs, lib, nixpkgs, ... }:

{
  # there's a lot of other features for advanced configuration/troubleshooting
  # steam installs which are explained at https://nixos.wiki/wiki/Steam
  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true;
    dedicatedServer.openFirewall = true;
  };
}
