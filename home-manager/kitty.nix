{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  home.packages = [pkgs.kitty];
  home.file.".config/kitty/kitty.conf".source = ../config/kitty/kitty.conf;
}
