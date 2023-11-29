{ config, pkgs, lib, inputs, ... }:

{
  imports = [
    ./x11-base.nix
    ./wezterm.nix
    ./gtk.nix
    ./qt.nix
  ];
  xsession.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    config = ./xmonad-config/xmonad.hs;
  };
}
