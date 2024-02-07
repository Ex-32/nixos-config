{ config, pkgs, lib, inputs, ... }:

{
  home.packages = with pkgs; [
    discord
    element-desktop
    signal-desktop
    slack
  ];
}
