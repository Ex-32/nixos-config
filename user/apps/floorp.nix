{ config, pkgs, lib, inputs, ... }:

{
  home.packages = with pkgs; [
    floorp
    tridactyl-native
  ];
}
