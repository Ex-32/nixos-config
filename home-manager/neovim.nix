{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: let
  nvim-deps = pkgs.symlinkJoin {
    name = "nvim-deps";
    paths = with pkgs; [
      # plugin dependencies
      cargo # rust-tools
      clang # nvim-treesitter (and others)
      ghc # haskell-tools.nvim
      gnumake # telescope-fzf-native
      haskell-language-server # haskell-tools.nvim
      nodejs # nvim-treesitter
      rust-analyzer # rust-tools
      tree-sitter # nvim-treesitter
      # language servers (lspconfig)
      arduino-language-server
      clang-tools # (clangd)
      cmake-language-server
      fortls
      gopls
      kotlin-language-server
      lua-language-server
      nixd
      nodePackages.bash-language-server
      nodePackages.pyright
      nodePackages.typescript-language-server
      python311Packages.jedi-language-server
      texlab
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
    paths = [pkgs.neovim-unwrapped];
    nativeBuildInputs = [pkgs.makeWrapper];
    postBuild = ''
      wrapProgram $out/bin/nvim \
        --suffix PATH : ${nvim-deps}/bin
    '';
  };
in {
  home.packages = [nvim-with-deps];
  home.sessionVariables.EDITOR = "nvim";
  home.file.".config/nvim/init.lua".source = ../config/nvim/init.lua;
}
