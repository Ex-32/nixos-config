{ config, pkgs, lib, nixpkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    xorg.xauth
  ];
  services.xserver = {
    enable = true;
    autorun = false;
    displayManager.startx.enable = true;
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config = builtins.readFile ./xmonad-config/xmonad.hs;
    };
  };
}
