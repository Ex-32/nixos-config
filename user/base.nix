{ config, pkgs, lib, inputs, ... }:

{
  home.packages = with pkgs; [
    home-manager
  ];

  programs.home-manager.enable = true;
  home.stateVersion = "23.11";
}
