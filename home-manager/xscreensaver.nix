{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  imports = [
    ./x11-base.nix
  ];
  services.xscreensaver = {
    enable = true;
    settings = {
    };
  };
}
