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
  ];

  home.packages =
    [
      pkgs.obsidian
      pkgs.zathura
    ]
    ++ lib.optionals (builtins.elem pkgs.system lib.platforms.linux) [
      libreoffice-with-deps
      pkgs.onlyoffice-desktopeditors
      pkgs.foliate
    ];
}
