{ config, pkgs, lib, inputs, ... }:

{
  xsession = {
    enable = true;
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config = builtins.readFile ./xmonad-config/xmonad.hs;
    };
  };
}
