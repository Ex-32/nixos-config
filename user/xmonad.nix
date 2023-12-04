{ config, pkgs, lib, inputs, ... }:

{
  imports = [
    ./x11-base.nix
    ./wezterm.nix
    ./gtk.nix
    ./qt.nix
    ./xscreensaver.nix
    ./picom.nix
    ./xmobar.nix
  ];

  home.packages = with pkgs; [
    feh # for setting the wallpaper
  ];
  home.file."documents/pictures/wallpaper.png".source = let 
    wallpaper = inputs.nix-wallpaper.packages."${pkgs.system}".default.override {
      preset = "catppuccin-mocha-rainbow";
      width = 3840;
      height = 2160;
      logoSize = 42;
      backgroundColor = "#11111b";
    }; 
  in "${wallpaper}/share/wallpapers/nixos-wallpaper.png";

  xsession.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    config = ./xmonad/xmonad.hs;
  };
}
