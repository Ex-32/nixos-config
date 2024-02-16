{ config, pkgs, lib, inputs, ... }:

{
  programs.obs-studio = {
    enable = true;
    plugins = with pkgs.obs-studio-plugins; [
      wlrobs
      # FIXME: currently broken, re-add later
      # obs-backgroundremoval
      obs-pipewire-audio-capture
    ];
  };
}
