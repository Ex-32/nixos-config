{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  allowedUnfree = [
    "aspell-dict-en-science"
  ];

  programs.emacs = let
    # from https://nixos.wiki/wiki/TexLive#Combine_Sets
    tex = pkgs.texlive.combine {
      inherit
        (pkgs.texlive)
        amsmath
        capt-of
        dvipng # for preview and export as html
        dvisvgm
        hyperref
        scheme-basic
        ulem
        wrapfig
        ;

      # additional packages
      inherit
        (pkgs.texlive)
        cancel
        ;

      #(setq org-latex-compiler "lualatex")
      #(setq org-preview-latex-default-process 'dvisvgm)
    };

    emacs-deps = pkgs.symlinkJoin {
      name = "emacs-deps";
      paths =
        (with pkgs; [
          (aspellWithDicts (dicts: with dicts; [en en-computers en-science]))
          alejandra
          cargo
          clang-tools
          cmake-language-server
          ghc # needed for hls
          gnumake
          gopls
          haskell-language-server
          lua-language-server
          nixd
          nodePackages.bash-language-server
          pandoc
          pyright
          rust-analyzer
          texlab
          vscode-langservers-extracted
          shellcheck
          shfmt
          editorconfig-core-c
        ])
        ++ [
          tex
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

    emacs-with-deps = let
      emacs = pkgs.emacs-pgtk;
    in
      pkgs.symlinkJoin {
        name = "emacs-with-deps";
        paths = [emacs];
        nativeBuildInputs = [pkgs.makeWrapper];
        postBuild = ''
          wrapProgram $out/bin/emacs \
            --suffix PATH : ${emacs-deps}/bin
        '';

        # NOTE: nix-doom-emacs checks this info, so it's being passed through
        # from the base emacs packages
        version = emacs.version;
        src = emacs.src;
        meta = emacs.meta;
      };
  in {
    enable = true;
    package = emacs-with-deps;
  };

  home.file = {
    ".emacs.d/init.el".enable = false;
    ".config/doom" = {
      source = ../config/doom;
      recursive = true;
    };
  };
}
