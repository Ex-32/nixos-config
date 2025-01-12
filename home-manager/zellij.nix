{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  programs.zellij.enable = true;
  home.file.".config/zellij/config.kdl".source = ../config/zellij/config.kdl;
}
