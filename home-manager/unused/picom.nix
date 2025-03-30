{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  services.picom = {
    enable = true;
    backend = "glx";
    vSync = true;
    shadow = false;
    shadowExclude = [
      "window_type *= 'menu'"
      "name ~= 'Firefox$'"
      "name ~= 'tint2$'"
      "name ~= 'taffybar$'"
    ];
  };
}
