{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  allowedUnfree = [
    "discord"
    "signal-desktop"
  ];

  home.packages = with pkgs; [
    deltachat-desktop
    discord
    element-desktop
    signal-desktop
  ];
}
