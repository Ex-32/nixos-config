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
    "obsidian"
    "slack"
    "zoom"
  ];

  home.packages = with pkgs;
    [
      obsidian
      slack
      zathura
      zoom-us
    ]
    ++ lib.optionals (builtins.elem pkgs.system lib.platforms.linux) [
      foliate
      font-manager
      libreoffice-with-deps
      nextcloud-client
      onlyoffice-desktopeditors
      teams-for-linux
    ];
}
