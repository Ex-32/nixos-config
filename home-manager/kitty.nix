{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  imports = [
    ./nerdfont.nix
    ./xonsh.nix
  ];

  home.packages = with pkgs; [
    kitty
    zellij
  ];
  home.file.".config/kitty/kitty.conf".source = ../config/kitty/kitty.conf;
}
