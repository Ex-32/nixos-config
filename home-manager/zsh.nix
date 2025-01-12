{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: let
  p10k-path = ".config/zsh/.p10k.zsh";
in {
  home.file."${p10k-path}".source = ../config/zsh/p10k.zsh;

  programs = {
    zsh = let
      p10k-source = ''[ "$TERM" = 'linux' ] || source'';
    in {
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

      initExtraFirst = ''
        # make ctrl+arrow-key work as expected
        # (for kitty at least, may vary with terminal emulators)
        bindkey "^[[1;5C" forward-word
        bindkey "^[[1;5D" backward-word

        ${p10k-source} ${pkgs.zsh-powerlevel10k}/share/zsh-powerlevel10k/powerlevel10k.zsh-theme
      '';
      initExtra = ''
        ns() {
            nix_shell=pure
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

        ${p10k-source} ~/${p10k-path}
      '';
    };
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
