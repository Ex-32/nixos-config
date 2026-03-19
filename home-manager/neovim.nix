{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  programs.neovim = {
    enable = true;
    defaultEditor = true;

    plugins = with pkgs.vimPlugins; [
      nvim-treesitter.withAllGrammars
      vim-sleuth
    ];

    extraPackages = with pkgs; [
      # plugin dependencies
      cargo # rustaceanvim
      clang # nvim-treesitter (and others)
      gnumake # telescope-fzf-native
      lean4 # lean.nvim
      rust-analyzer # rustaceanvim
      # language servers (lspconfig)
      arduino-language-server
      clang-tools # (clangd)
      cmake-language-server
      erlang-language-platform
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
  };

  home.file.".config/nvim" = {
    source = ../config/nvim;
    recursive = true;
  };
}
