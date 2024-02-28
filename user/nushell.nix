{ config, pkgs, lib, inputs, ... }:

{
  programs.nushell = {
    enable = true;
    extraConfig = ''
      $env.config = {
        show_banner: false,
        completions: {
          case_sensitive: true # case-sensitive completions
          quick: true    # set to false to prevent auto-selecting completions
          partial: true    # set to false to prevent partial filling of the prompt
          algorithm: "prefix"    # prefix or fuzzy
          external: {
            # set to false to prevent nushell looking into $env.PATH to find more suggestions
            enable: true 
            # set to lower can improve completion performance at the cost of omitting some options
            max_results: 100 
          }
        }
      }
      def l [arg: string = "."] { 
        ls -al $arg | select mode user group size modified type name | sort-by name 
      }
      def ll [arg: string = "."] {
        ls -al $arg | select mode user group size modified type name target | sort-by name
      }

      def ns [...args: string] {
        nix shell (echo ...$args | each { |item| echo ["nixpkgs#", $item] | str join })
      }

      alias nrs = sudo nixos-rebuild switch
      alias sc  = sudo systemctl
      alias scu = systemctl --user
    '';
  };
}
