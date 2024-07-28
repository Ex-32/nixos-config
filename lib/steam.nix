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
  ];

  # there's a lot of other features for advanced configuration/troubleshooting
  # steam installs which are explained at https://nixos.wiki/wiki/Steam
  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true;
    dedicatedServer.openFirewall = true;
  };

  environment.systemPackages = with pkgs; [
    steamtinkerlaunch
    winetricks
    protontricks
  ];

  environment.variables = {
    STEAM_EXTRA_COMPAT_TOOL_PATHS = "${pkgs.runCommand "steamtinkerlaunch-compat" {} ''
      mkdir -p $out/SteamTinkerLaunch
      cd $out/SteamTinkerLaunch
      ln -s ${pkgs.steamtinkerlaunch}/bin/steamtinkerlaunch
    ''}";
  };
}
