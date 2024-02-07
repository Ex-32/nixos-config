{ config, pkgs, lib, inputs, ... }:

{
  home.packages = with pkgs; [
    ffmpeg
    gimp
    inkscape
    rawtherapee
    vlc
  ];
}
