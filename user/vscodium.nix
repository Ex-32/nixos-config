{ config, pkgs, lib, inputs, ... }:

{
  programs.vscode = {
    enable = true;
    # package = pkgs.vscodium;
    extensions = with pkgs.vscode-extensions; [
      catppuccin.catppuccin-vsc
      catppuccin.catppuccin-vsc-icons
      ms-vscode-remote.remote-containers
    ] ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [

    ];
  };
}
