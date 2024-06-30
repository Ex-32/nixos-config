{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  imports = [inputs.nix-doom-emacs.hmModule];

  programs.doom-emacs = let
    config-path = ../config/emacs;
    emacs-deps = pkgs.symlinkJoin {
      name = "emacs-deps";
      paths = with pkgs; [
        (aspellWithDicts (dicts: with dicts; [en en-computers en-science]))
        clang-tools
        ghc # needed for hls
        gnumake
        gopls
        haskell-language-server
        lua-language-server
        nodePackages.bash-language-server
        pandoc
        pyright
        rust-analyzer
        texlab
        vscode-langservers-extracted
      ];

      # symlinkJoin can't handle symlinked dirs and nodePackages
      # symlinks ./bin -> ./lib/node_modules/.bin/.
      postBuild = ''
        for f in $out/lib/node_modules/.bin/*; do
           path="$(readlink --canonicalize-missing "$f")"
           ln -s "$path" "$out/bin/$(basename $f)"
        done
      '';
    };

    emacs-with-deps = pkgs.symlinkJoin {
      name = "emacs-with-deps";
      paths = [pkgs.emacs];
      nativeBuildInputs = [pkgs.makeWrapper];
      postBuild = ''
        wrapProgram $out/bin/emacs \
          --suffix PATH : ${emacs-deps}/bin
      '';

      # NOTE: nix-doom-emacs checks this info, so it's being passed through
      # from the base emacs packages
      version = pkgs.emacs.version;
      src = pkgs.emacs.src;
      meta = pkgs.emacs.meta;
    };
  in {
    enable = true;
    emacsPackage = emacs-with-deps;
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

  # services.emacs.enable = true;
  # xdg.desktopEntries.emacs = {
  #   name = "Doom Emacs";
  #   genericName = "Text Editor";
  #   exec = "${config.programs.doom-emacs.package}/bin/emacsclient -c";
  #   terminal = false;
  #   categories = ["Utility" "TextEditor"];
  #   mimeType = ["text/plain"];
  #   icon = "${config.programs.doom-emacs.package}/share/icons/hicolor/scalable/apps/emacs.svg";
  # };

  home.file.".emacs.d/init.el".enable = false;
}
