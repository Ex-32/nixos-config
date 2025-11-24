{
  config,
  osConfig,
  pkgs,
  lib,
  inputs,
  ...
}: {
  programs = {
    fish = {
      enable = true;
      shellInit =
        # fish
        ''
          set -g fish_greeting
        '';
      interactiveShellInit =
        # fish
        ''
          fish_vi_key_bindings
          set -g fish_cursor_insert line
          set -g fish_cursor_replace_one underscore
          set -g fish_cursor_visual block
        '';
      shellAliases = {
        "cd" = "z";
      };
      functions = let
        nom = lib.getExe pkgs.nix-output-monitor;
      in {
        ns.body =
          # fish
          ''
            for arg in $argv
              set -fa pkgs "nixpkgs#$arg"
            end
            ${nom} shell $pkgs
          '';
        nsau.body =
          # fish
          ''
            for arg in $argv
              set -fa pkgs "nixpkgs#$arg"
            end
            NIXPKGS_ALLOW_UNFREE=1 ${nom} shell --impure $pkgs
          '';
        __exit_hook = {
          onEvent = "fish_exit";
          body = "clear";
        };
      };
    };

    carapace.enable = true;
    zoxide.enable = true;
    direnv = {
      enable = true;
      nix-direnv.enable = true;
      config = {
        global = {
          warn_timeout = "-1s";
          hide_env_diff = true;
        };
      };
    };
  };
}
