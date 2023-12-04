{ config, pkgs, lib, inputs, ... }:

{
  services.picom = {
    enable = true;
    backend = "glx";
    activeOpacity = 1.0;
    inactiveOpacity = 0.9;
    vSync = true;
    shadow = true;
    shadowExclude = [
      "window_type *= 'menu'"
      "name ~= 'Firefox$'"
    ];
  };
}
