{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  allowedUnfree = [
    "discord"
    "slack"
  ];

  home.packages = with pkgs; [
    discord
    nheko
    signal-desktop
    slack
  ];
}
