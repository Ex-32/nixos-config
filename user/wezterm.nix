{ config, pkgs, lib, inputs, ... }:

{
  imports = [
    ./nerdfont.nix
  ];

  programs.wezterm = {
    enable = true;
    extraConfig = (builtins.readFile ../config/wezterm/wezterm.lua);
  };
}
