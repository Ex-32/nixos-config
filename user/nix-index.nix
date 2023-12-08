{ config, pkgs, lib, inputs, ... }:

{
  programs.nix-index = {
    enable = true;
    enableBashIntegration = config.programs.bash.enable;
    enableFishIntegration = config.programs.fish.enable;
    enableZshIntegration = config.programs.zsh.enable;
  };
}
