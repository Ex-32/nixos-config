{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: let
  libreoffice-deps = pkgs.symlinkJoin {
    name = "libreoffice-deps";
    paths = with pkgs; [
      hunspell
      hunspellDicts.en_US-large
    ];
  };

  libreoffice-with-deps = pkgs.symlinkJoin {
    name = "libreoffice-with-deps";
    paths = [pkgs.libreoffice-qt];
    buildInputs = [pkgs.makeWrapper];
    postBuild = ''
      for f in $out/bin/*; do
        wrapProgram $f \
          --suffix PATH : ${libreoffice-deps}/bin
      done
    '';
  };
in {
  allowedUnfree = [
    "anytype"
    "anytype-heart"
    "slack"
    "zoom"
  ];

  home.packages = with pkgs; [
    # FIXME: anytype implicitly depends on a working mDNS stack
    anytype
    foliate
    font-manager
    libreoffice-with-deps
    nextcloud-client
    onlyoffice-desktopeditors
    presenterm
    slack
    teams-for-linux
    zathura
    zoom-us
  ];
}
