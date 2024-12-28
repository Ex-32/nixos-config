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
    # nheko
    signal-desktop
  ];
}
