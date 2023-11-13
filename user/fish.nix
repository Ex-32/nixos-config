{ config, pkgs, lib, inputs, ... }:

{
  imports = [
    ./shell-base.nix
    ./tmux.nix
    ./starship.nix
  ];

  programs.fish = {
    enable = true;
    shellInit = ''
      set -g fish_greeting
    '';
    shellAliases = {
        cd = "z";
    };
    functions = {
      leak.body = ''
        fish -c "$argv &> /dev/null &"
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
      {
        name = "bass";
        src = pkgs.fetchzip {
          url = "https://github.com/edc/bass/archive/master.zip";
          sha256 = "h6NM7BMFnFgyGL0rwiUq8UPYEDpnivhMjfHQJua06N8=";
        };
      }
    ];
  };
}
