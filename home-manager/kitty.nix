{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  home.packages = with pkgs; [
    kitty
    zellij
  ];
  home.file.".config/kitty/kitty.conf".source = ../config/kitty/kitty.conf;
}
