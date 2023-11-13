{ config, pkgs, lib, inputs, ... }:

{
  home.packages = with pkgs; [
    neovim
    nixd
    nodejs
  ];
}
