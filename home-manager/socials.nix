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

  home.packages = with pkgs;
    [
      discord
      signal-desktop
    ]
    ++ (lib.optionals (builtins.elem pkgs.system lib.platforms.linux) (with pkgs; [
      nheko
    ]));
}
