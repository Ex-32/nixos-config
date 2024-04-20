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
    element-desktop
    signal-desktop
    slack
  ];
}
