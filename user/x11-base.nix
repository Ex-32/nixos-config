{ config, pkgs, lib, inputs, ... }:

{
  xsession.enable = true;
  home.packages = with pkgs; [
    brightnessctl
    playerctl
    rofi
    scrot
    tdrop
    xclip
  ];
}
