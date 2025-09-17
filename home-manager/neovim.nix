{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  home.packages = let
    nvim-deps = pkgs.symlinkJoin {
      name = "nvim-deps";
      paths = with pkgs; [
        # plugin dependencies
        cargo # rustaceanvim
        clang # nvim-treesitter (and others)
        gnumake # telescope-fzf-native
        lean4 # lean.nvim
        nodejs # nvim-treesitter
        rust-analyzer # rustaceanvim
        tree-sitter # nvim-treesitter
        # language servers (lspconfig)
        arduino-language-server
        clang-tools # (clangd)
        cmake-language-server
        erlang-ls
        fortls
        ghc # needed for hls
        gopls
        haskell-language-server
        lua-language-server
        marksman
        nixd
        nodePackages.bash-language-server
        pyright
        texlab
        typescript-language-server
        vscode-langservers-extracted
        # sources ({null,none}-ls)
        alejandra
        clang-tools # (clang-format)
        mypy
        nodePackages.prettier
        statix
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

    nvim-with-deps = pkgs.symlinkJoin {
      name = "nvim-with-deps";
      paths = [pkgs.neovim];
      nativeBuildInputs = [pkgs.makeWrapper];
      postBuild = ''
        wrapProgram $out/bin/nvim \
          --prefix PATH : ${nvim-deps}/bin \
          --add-flags "--cmd 'set shell=${pkgs.bash}/bin/bash'"
      '';
    };
  in [nvim-with-deps];
  home.sessionVariables.EDITOR = "nvim";
  home.file.".config/nvim" = {
    source = ../config/nvim;
    recursive = true;
  };
}
