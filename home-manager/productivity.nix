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
  home.packages = [
    libreoffice-with-deps
    pkgs.onlyoffice-bin
    pkgs.zathura
  ];
}
