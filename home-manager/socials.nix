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
    "slack"
  ];

  home.packages = with pkgs;
    [
      discord
      signal-desktop
      slack
    ]
    ++ (lib.optionals (builtins.elem pkgs.system lib.platforms.linux) (with pkgs; [
      nheko
      teams-for-linux
    ]));
}
