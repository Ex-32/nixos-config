{ config, pkgs, lib, inputs, ... }:

{
  imports = [
   ./x11-base.nix
  ]
  programs.xmobar = {
    enable = true;
    extraConfig = builtins.readFile ./xmonad-config/xmobar.hs
  };
}
