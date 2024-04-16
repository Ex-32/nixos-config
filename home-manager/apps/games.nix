{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  home.packages = with pkgs; [
    endless-sky
    prismlauncher
    superTuxKart
  ];
}
