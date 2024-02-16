{ config, pkgs, lib, inputs, osConfig, ... }:

{
  programs.home-manager.enable = true;

  # there's probably a more correct way to do this, but since $MANPATH gets set
  # anyway, there's no need to create this file, and $HOME clutter is my
  # personal enemy
  home.file.".manpath".enable = false;

  # stateVersion mismatches between nixpkgs and home-manager are... *bad*
  home.stateVersion = osConfig.system.stateVersion;
}
