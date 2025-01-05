{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  home.packages = [pkgs.zellij];

  programs.ghostty = {
    enable = true;
    settings = {
      auto-update = "off";
      background-opacity = 0.8;
      font-family = "FiraCode Nerd Font Light";
      font-family-bold = "FiraCode Nerd Font Med";
      font-size = 16;
      gtk-single-instance = true;
      minimum-contrast = 1;
      theme = "cyberwave";
      window-decoration = false;
    };
    themes.cyberwave = {
      background = "100c00";
      cursor-color = "ff006e";
      foreground = "abf9fd";
      palette = [
        "0=#252726"
        "1=#ff006e"
        "2=#00b300"
        "3=#ace700"
        "4=#4105fb"
        "5=#9c00fc"
        "6=#00b0df"
        "7=#9a8898"
        "8=#848484"
        "9=#ff1212"
        "10=#00d500"
        "11=#d6e800"
        "12=#7000fc"
        "13=#cd00f0"
        "14=#25d0fe"
        "15=#bdadb8"
      ];
      selection-background = "ff006e";
      selection-foreground = "000000";
    };
  };
}
