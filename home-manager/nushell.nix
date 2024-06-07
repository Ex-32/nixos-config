{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  programs.nushell = {
    enable = true;
    configFile.source = ../config/nushell/config.nu;
    envFile.source = ../config/nushell/env.nu;
  };
}
