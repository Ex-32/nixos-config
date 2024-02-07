{ config, pkgs, lib, inputs, osConfig, ... }:

{
  programs.home-manager.enable = true;
  home.file.".manpath".enable = false;
  home.stateVersion = osConfig.system.stateVersion;
}
