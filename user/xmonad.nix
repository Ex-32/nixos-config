{ config, pkgs, lib, inputs, ... }:

{
  xsession.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    config = pkgs.writeText "xmonad.hs" ''
      import XMonad
      main = xmonad defaultConfig
          { terminal    = "wezterm"
          , modMask     = mod4Mask
          , borderWidth = 3
          }
    '';
  };
}
