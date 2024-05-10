{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  imports = [inputs.nix-doom-emacs.hmModule];

  home.packages = with pkgs; [
    (aspellWithDicts (dicts: with dicts; [en en-computers en-science]))
    clang-tools
    gnumake
    haskell-language-server
    nodePackages.bash-language-server
    vscode-langservers-extracted
  ];

  programs.doom-emacs = let
    config-path = ../config/emacs;
  in {
    enable = true;
    doomPrivateDir = pkgs.buildEnv {
      name = "doom-config";
      paths = [
        config-path
      ];
    };
    doomPackageDir = pkgs.stdenv.mkDerivation {
      name = "doom-without-config";
      src = builtins.path {
        path = config-path;
        name = "doom-private-dir-filtered";
        filter = path: type:
          builtins.elem (baseNameOf path) ["init.el" "packages.el"];
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
  xdg.desktopEntries.emacs = {
    name = "Doom Emacs";
    genericName = "Text Editor";
    exec = "${config.programs.doom-emacs.package}/bin/emacsclient -c";
    terminal = false;
    categories = ["Utility" "TextEditor"];
    mimeType = ["text/plain"];
    icon = "${config.programs.doom-emacs.package}/share/icons/hicolor/scalable/apps/emacs.svg";
  };

  home.file.".emacs.d/init.el".enable = false;
}
