{ config, pkgs, lib, inputs, ... }:

{
  imports = [ inputs.nix-doom-emacs.hmModule ];

  home.packages = with pkgs; [
    (pkgs.aspellWithDicts (dicts: with dicts; [ en en-computers en-science ]))
    pkgs.nodePackages.vscode-langservers-extracted
    pkgs.nodePackages.unified-language-server
  ];

  programs.doom-emacs = {
    enable = true;
    doomPrivateDir = ../config/emacs;
    doomPackageDir = pkgs.stdenv.mkDerivation {
      name = "doom-without-config";
      src = builtins.path {
        path = ./emacs;
        name = "doom-private-dir-filtered";
        filter = path: type:
          builtins.elem (baseNameOf path) [ "init.el" "packages.el" ];
      };
      installPhase = ''
        mkdir $out
        cp init.el $out
        cp packages.el $out
        touch $out/config.el
      '';
    };
  };

  services.emacs.enable = true;
}
