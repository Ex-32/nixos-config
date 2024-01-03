{ config, pkgs, lib, inputs, osConfig, ... }:

{
  programs.home-manager.enable = true;
  home.stateVersion = osConfig.system.stateVersion;
}
