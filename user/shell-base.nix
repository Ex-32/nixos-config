{ config, pkgs, lib, inputs, ... }:

{
  programs.zoxide.enable = true;
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };
}
