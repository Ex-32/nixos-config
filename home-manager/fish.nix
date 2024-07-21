{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  imports = [
    ./tmux.nix
    ./starship.nix
  ];

  home.packages = with pkgs; [
    babelfish
  ];

  programs.fish = {
    enable = true;
    shellInit =
      # fish
      ''
        set -g fish_greeting
      '';
    # interactiveShellInit = let
    #   pokemon-colorscripts = pkgs.nur.repos.Ex-32.pokemon-colorscripts;
    # in
    #   # fish
    #   ''
    #     fish_vi_key_bindings
    #     set -g fish_cursor_insert line
    #     set -g fish_cursor_replace_one underscore
    #     set -g fish_cursor_visual block
    #     set -l pokemon_terms \
    #       "xterm-kitty" \
    #       "xterm-256color"
    #     if test $SHLVL -le 1 ; and contains $TERM $pokemon_terms
    #       ${pokemon-colorscripts}/bin/pokemon-colorscripts --random
    #     end
    #   '';
    functions = {
      i.body =
        # fish
        ''
          if test -z "$argv"
            i .
          else
            for arg in $argv
              if test -d $arg
                lsd -lAh --date relative --no-symlink $arg
              else if file --mime-encoding (realpath $arg) | grep 'binary$' &> /dev/null
                file $arg
              else
                bat --paging never $arg
              end
            end
          end
        '';
      leak.body =
        # fish
        ''
          fish -c "$argv &> /dev/null &"
        '';
      ns.body =
        # fish
        ''
          for arg in $argv
            set -fa pkgs "nixpkgs#$arg"
          end
          ${pkgs.nix-output-monitor}/bin/nom shell $pkgs
        '';
      __exit_hook = {
        onEvent = "fish_exit";
        body = "clear";
      };
    };
  };

  programs.carapace.enable = true;
  programs.zoxide.enable = true;
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
    config = {
      global = {
        warn_timeout = "-1s";
        strict_env = true;
        hide_env_diff = true;
      };
    };
  };
}
