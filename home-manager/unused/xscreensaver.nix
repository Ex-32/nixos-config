{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  services.xscreensaver = {
    enable = true;
    settings = {
    };
  };
}
