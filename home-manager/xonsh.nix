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
    ".config/xonsh/rc.xsh".text = lib.strings.concatStrings [
      # python
      ''
        # The SQLite history backend saves command immediately
        # unlike JSON backend that save the commands at the end of the session.
        # https://xon.sh/envvars.html#histcontrol
        $XONSH_HISTORY_FILE = $XDG_DATA_HOME + "/xonsh/history.sqlite"
        $XONSH_HISTORY_BACKEND = "sqlite"
        $HISTCONTROL = "erasedups"

        # I'm not a xonsh developer, I don't care about stack traces.
        $XONSH_SHOW_TRACEBACK = False

        # HACK: https://github.com/NixOS/nixpkgs/issues/276326
        $PATH = [
            path for path in $PATH
            if not ((p"" / path / "xonsh").exists() and (p"" / path).parts[1] == "nix")
        ]

        import json
        aliasFile = p"$XDG_CONFIG_HOME/xonsh/aliases.json"
        if aliasFile.exists():
            for alias, expansion in json.loads(aliasFile.read_text()).items():
                aliases[alias] = expansion

        for i in range(1, 10):
            aliases["." + ("." * i)] = "cd " + ("../" * i)

        def ns(args):
            args = [x if x[0:2] == "--" else f"nixpkgs#{x}" for x in args]
            @(["nom", "shell", *args, "--command", "xonsh"])

        aliases["ns"] = ns
        del ns
      ''

      # load carapace completions
      # https://carapace-sh.github.io/carapace-bin/setup.html#xonsh
      (lib.strings.optionalString config.programs.carapace.enable
        # python
        ''
          $CARAPACE_BRIDGES='zsh,fish,bash,inshellisense' # optional
          $COMPLETIONS_CONFIRM=True
          exec($(carapace _carapace))
        '')

      # load zoxide
      # https://github.com/ajeetdsouza/zoxide?tab=readme-ov-file#installation
      (lib.strings.optionalString config.programs.zoxide.enable
        # python
        ''
          execx($(zoxide init xonsh), 'exec', __xonsh__.ctx, filename='zoxide')
          aliases["cd"] = "z"
        '')

      # load direnv
      # https://github.com/74th/xonsh-direnv
      (lib.strings.optionalString config.programs.direnv.enable
        # python
        ''
          # xontrib load direnv
        '')
    ];
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
