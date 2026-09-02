{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  allowedUnfree = [
    "discord"
    "discord-unwrapped"
    "signal-desktop"
  ];

  home.packages = with pkgs; [
    deltachat-desktop
    discord
    element-desktop
    signal-desktop
  ];
}
