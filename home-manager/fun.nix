{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  allowedUnfree = [
    "pridecat" # licensed under permissive **non-commercial** license
  ];

  home.packages = with pkgs; [
    cava
    cool-retro-term
    fastfetch
    genact
    lolcat
    nur.repos.Ex-32.pokemon-colorscripts
    pridecat
  ];
}
