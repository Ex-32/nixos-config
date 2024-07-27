{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  services.syncthing.enable = true;
}
