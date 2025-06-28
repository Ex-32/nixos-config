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
      cinny-desktop
    ]
    ++ (lib.optionals (builtins.elem pkgs.system lib.platforms.linux) (with pkgs; [
      teams-for-linux
    ]));
}
