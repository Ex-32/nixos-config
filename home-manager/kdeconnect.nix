{
  config,
  pkgs,
  lib,
  inputs,
  osConfig,
  ...
}: {
  services.kdeconnect = {
    enable = true;
    indicator = true;
  };
}
