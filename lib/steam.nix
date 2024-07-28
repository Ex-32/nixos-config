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
    extraCompatPackages = [
      (pkgs.stdenvNoCC.mkDerivation {
        pname = pkgs.steamtinkerlaunch.pname;
        version = pkgs.steamtinkerlaunch.version;

        src = pkgs.steamtinikerlaunch;

        outputs = [
          "steamcompattool"
        ];

        dontUnpack = true;
        dontBuild = true;
        installPhase = ''
          mkdir -p $steamcompattool
          ln -s ./bin/steamtinikerlaunch $steamcompattool/steamtinikerlaunch
        '';
      })
    ];
  };

  environment.systemPackages = with pkgs; [
    steamtinkerlaunch
    winetricks
    protontricks
  ];
}
