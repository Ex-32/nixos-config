{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  services.podman.enable = true;
  home.packages = [
    pkgs.podman-compose
    pkgs.winboat
  ];
}
