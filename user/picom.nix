{ config, pkgs, lib, inputs, ... }:

{
  services.picom = {
    enable = true;
    backend = "glx";
    vSync = true;
    shadow = true;
    shadowExclude = [
      "window_type *= 'menu'"
      "name ~= 'Firefox$'"
    ];
  };
}
