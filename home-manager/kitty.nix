{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  imports = [
    ./xonsh.nix
  ];

  home.packages = with pkgs; [
    kitty
    zellij
  ];
  home.file.".config/kitty/kitty.conf".source = ../config/kitty/kitty.conf;
}
