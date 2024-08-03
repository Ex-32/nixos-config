{
  config,
  osConfig,
  pkgs,
  lib,
  inputs,
  ...
}: {
  home.file = {
    ".config/xonsh/aliases.json".text =
      builtins.toJSON osConfig.environment.shellAliases;
    ".config/xonsh/rc.xsh".source = pkgs.substituteAll {
      src = ../config/xonsh/rc.xsh;

      # load carapace completions
      # https://carapace-sh.github.io/carapace-bin/setup.html#xonsh
      carapace_init =
        lib.strings.optionalString config.programs.carapace.enable
        # python
        ''
          $CARAPACE_BRIDGES='zsh,fish,bash,inshellisense' # optional
          $COMPLETIONS_CONFIRM=True
          exec($(carapace _carapace))
        '';

      # load zoxide
      # https://github.com/ajeetdsouza/zoxide?tab=readme-ov-file#installation
      zoxide_init =
        lib.strings.optionalString config.programs.zoxide.enable
        # python
        ''
          execx($(zoxide init xonsh), 'exec', __xonsh__.ctx, filename='zoxide')
          aliases["cd"] = "z"
        '';

      # load direnv
      # https://github.com/74th/xonsh-direnv
      direnv_init =
        lib.strings.optionalString config.programs.direnv.enable
        # python
        ''
          xontrib load direnv
        '';
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
        # strict_env = true;
        hide_env_diff = true;
      };
    };
  };
}
