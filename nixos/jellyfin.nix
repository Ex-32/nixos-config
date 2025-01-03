{
  config,
  pkgs,
  lib,
  nixpkgs,
  ...
}: let
  jellyfin-dir = "/persist/volatile/jellyfin";
in {
  services.jellyfin = {
    enable = true;
    openFirewall = true;
    dataDir = jellyfin-dir + "/jellyfin";
    cacheDir = jellyfin-dir + "/cache";
  };
  environment.systemPackages = with pkgs; [
    jellyfin
    jellyfin-web
    jellyfin-ffmpeg
  ];
}
