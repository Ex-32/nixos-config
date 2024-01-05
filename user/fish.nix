{ config, pkgs, lib, inputs, ... }:

{
  imports = [
    ./tmux.nix
    ./starship.nix
  ];

  home.packages = with pkgs; [
    babelfish
  ]; 

  programs.fish = {
    enable = true;
    shellInit = ''
      set -g fish_greeting
    '';
    functions = {
      leak.body = ''
        fish -c "$argv &> /dev/null &"
      '';
      ns.body = ''
        for arg in $argv
          set -fa pkgs "nixpkgs#$arg"
        end
        nix shell $pkgs
      '';
      onExit = {
        onEvent = "fish_exit";
        body = "clear";
      };
    };
    plugins = [
      {
        name = "bang-bang";
        src = pkgs.fetchzip {
          url = "https://github.com/oh-my-fish/plugin-bang-bang/archive/master.zip";
          sha256 = "oPPCtFN2DPuM//c48SXb4TrFRjJtccg0YPXcAo0Lxq0=";
        };
      }
    ];
  };

  programs.zoxide.enable = true;
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };
}
