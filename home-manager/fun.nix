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
    genact
    lolcat
    nur.repos.Ex-32.pokemon-colorscripts
    pridecat
  ];
}
