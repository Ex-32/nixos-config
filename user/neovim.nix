{ config, pkgs, lib, inputs, ... }: let
  nvim-deps = pkgs.symlinkJoin {
    name = "nvim-deps";
    paths = with pkgs; [
      # plugin dependencies
      cargo         # rust-tools
      clang         # nvim-treesitter (and others)
      gnumake       # telescope-fzf-native
      rust-analyzer # rust-tools
      # language servers (lspconfig)
      clang-tools # (clangd)
      cmake-language-server
      gopls
      lua-language-server
      nixd
      nodePackages.pyright
      nodePackages.typescript-language-server
      texlab
      # sources ({null,none}-ls)
      alejandra
      clang-tools # (clang-format)
      mypy
      nodePackages.eslint
      nodePackages.prettier
      ruff
      shellcheck
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
    name = "nvim-with-lsps";
    paths = [ pkgs.neovim-unwrapped ];
    buildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/nvim \
        --suffix PATH : ${nvim-deps}/bin
    '';
  };
in {
  home.packages = [ nvim-with-deps ];
  home.sessionVariables.EDITOR = "nvim";
  home.file.".config/nvim/init.lua".source = ../config/nvim/init.lua;
}
