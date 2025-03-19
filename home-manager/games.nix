{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  options.local = {
    lutris.enable = lib.mkEnableOption "Lutris Gaming Platform";
  };

  config = lib.mkMerge [
    {
      allowedUnfree = [
        "dwarf-fortress"
      ];

      home.packages = with pkgs; [
        dwarf-fortress
        endless-sky
        mindustry
        prismlauncher
        # sauerbraten
        (sauerbraten.overrideAttrs (old: rec {
          installPhase = ''
            runHook preInstall

            mkdir -p $out/bin $out/share/icon/ $out/share/sauerbraten $out/share/doc/sauerbraten
            cp -r "../docs/"* $out/share/doc/sauerbraten/
            cp sauer_client sauer_server $out/share/sauerbraten/
            cp -r ../packages ../data $out/share/sauerbraten/
            ln -s $out/share/sauerbraten/data/cube.png $out/share/icon/sauerbraten.png

            makeWrapper $out/share/sauerbraten/sauer_server $out/bin/sauerbraten_server \
              --chdir "$out/share/sauerbraten"
            makeWrapper $out/share/sauerbraten/sauer_client $out/bin/sauerbraten_client \
              --chdir "$out/share/sauerbraten" \
              --add-flags "-q\''${HOME}/.config/sauerbraten"

            runHook postInstall
          '';
        }))

        superTuxKart
      ];
    }
    (lib.mkIf config.local.lutris.enable {
      home.packages = let
        lutris = pkgs.lutris.override {
          extraLibraries = pkgs: with pkgs; [];
          extraPkgs = pkgs: with pkgs; [];
          steamSupport = false;
        };
      in [lutris];
    })
  ];
}
