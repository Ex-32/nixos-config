{ config, pkgs, lib, inputs, ... }:

{
  programs.swaylock = {
    enable = true;
    package = pkgs.swaylock-effects;
    settings = {
      screenshots = true;
      clock = true;
      timestr = "%H:%M:%S";
      datestr = "%Y-%m-%d";
      indicator = true;
      indicator-radius = 350;
      indicator-thickness = 12;
      effect-blur = "8x5";
      ring-color = "cba6f7";
      ring-clear-color = "fab387";
      ring-ver-color = "74c7ec";
      ring-wrong-color = "f38ba8";
      key-hl-color = "45475a";
      bs-hl-color = "fab387";
      line-color = "00000000";
      line-clear-color = "00000000";
      line-caps-lock-color = "00000000";
      line-ver-color = "00000000";
      line-wrong-color = "00000000";
      inside-color = "00000000";
      inside-clear-color = "00000000";
      inside-caps-lock-color = "00000000";
      inside-ver-color = "00000000";
      inside-wrong-color = "00000000";
      separator-color = "00000000";
      text-color = "cba6f7";
      text-clear-color = "fab387";
      text-caps-lock-color = "f38ba8";
      text-ver-color = "74c7ec";
      text-wrong-color = "f38ba8";
      fade-in = 0.2;
      font = "Raleway";
    };
  };

}
