{ config, pkgs, lib, inputs, ... }:

{
  home.packages = with pkgs; [
    brightnessctl
    grim
    playerctl
    rofi-wayland
    slurp
    wl-clipboard
  ];

  services.mako = {
    enable = true;
    font = "Raleway 13";
    defaultTimeout = 5000;
    anchor = "bottom-right";
    width = 400;
    height = 400;
    backgroundColor = "#1e1e2e";
    textColor = "#cdd6f4";
    borderColor = "#cba6f7";
    progressColor = "#313244";
    extraConfig = ''
      [urgency=high]
      border-color=#f38ba8

      [urgency=low]
      border-color=#a6e3a1
    '';
  };
}
