{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  home = {
    packages = [pkgs.zellij];
    file.".config/zellij/config.kdl".source = ../config/zellij/config.kdl;
  };
}
