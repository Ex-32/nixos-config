{ config, pkgs, lib, inputs, ... }: let
  nvim-with-lsps = pkgs.symlinkJoin rec {
    name = "nvim-with-lsps";
    paths = with pkgs; [
      neovim-unwrapped

      cargo
      clang
      clang-tools
      cmake-language-server
      gopls
      lua-language-server
      mypy
      nixd
      nodePackages.eslint
      nodePackages.pyright
      nodePackages.typescript-language-server
      ruff
      rust-analyzer
      shellcheck
      texlab
    ];
    buildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/nvim \
        --suffix PATH : ${lib.makeBinPath paths}
    '';
  };
in {
  home.packages = [ nvim-with-lsps ];
  home.file.".config/nvim/init.lua".source = ../config/nvim/init.lua;
}
