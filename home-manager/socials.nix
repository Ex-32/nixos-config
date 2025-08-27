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
    discord
    signal-desktop
    element-desktop
  ];
}
