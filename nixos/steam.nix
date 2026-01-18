{
  config,
  pkgs,
  lib,
  nixpkgs,
  ...
}: {
  allowedUnfree = [
    "steam"
    "steam-original"
    "steam-run"
    "steam-unwrapped"
  ];

  # there's a lot of other features for advanced configuration/troubleshooting
  # steam installs which are explained at https://nixos.wiki/wiki/Steam
  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true;
    dedicatedServer.openFirewall = true;
    extraCompatPackages = [
      pkgs.steamtinkerlaunch.steamcompattool
    ];
  };

  environment.systemPackages = with pkgs; [
    protontricks
    steamtinkerlaunch
    winetricks
  ];
}
