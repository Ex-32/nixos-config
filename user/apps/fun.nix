{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  home.packages = with pkgs; [
    cava
    cool-retro-term
    lolcat
    pridecat
  ];
}
