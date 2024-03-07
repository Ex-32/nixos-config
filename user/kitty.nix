{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  imports = [
    ./nerdfont.nix
  ];

  home.packages = [pkgs.kitty];
  home.file.".config/kitty/kitty.conf".source = ../config/kitty/kitty.conf;
}
