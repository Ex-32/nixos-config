{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  imports = [
    ./starship.nix
  ];

  programs = {
    zsh = {
      enable = true;
      autocd = true;
      dotDir = ".config/zsh";
      history = let
        ZLONG_MAX = 9223372036854775807;
      in {
        expireDuplicatesFirst = true;
        extended = true;
        ignoreAllDups = true;
        ignorePatterns = ["rm *" "pkill *"];
        path = "${config.xdg.dataHome}/zsh/zsh_history";
        save = ZLONG_MAX;
        share = true;
        size = ZLONG_MAX;
      };

      initExtra =
        # sh
        ''
          # make ctrl+arrow-key work as expected
          # (for kitty at least, may vary with terminal emulators)
          bindkey "^[[1;5C" forward-word
          bindkey "^[[1;5D" backward-word

          ns() {
              local arg_list=""
              local nix_shell=pure
              for arg in $@ ; do
                  [[ "$arg" == "--impure" ]] && nix_shell=impure
                  if [[ "$arg" =~ "--.*" ]] ; then
                      arg_list="$arg_list '$arg'"
                  else
                      arg_list="$arg_list 'nixpkgs#$arg'"
                  fi
              done
              eval "env IN_NIX_SHELL=$nix_shell nix shell $arg_list"
          }

          _set_title_preexec() { print -Pn "\e]0;%~ | $1\a" }
          _set_title_precmd() { print -Pn "\e]0;%~ | zsh\a" }
          preexec_function+=(_set_title_preexec)
          precmd_functions+=(_set_title_precmd)
        '';
    };

    starship.enableZshIntegration = true;

    direnv = {
      enable = true;
      enableZshIntegration = true;
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
