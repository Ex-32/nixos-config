{
  config,
  osConfig,
  pkgs,
  lib,
  inputs,
  ...
}: {
  config = lib.mkMerge [
    {
      programs = {
        nushell = {
          enable = true;
          configFile.source = ../config/nushell/config.nu;
          envFile.source = ../config/nushell/env.nu;
          shellAliases = let
            # exclude problematic or undesired aliases from those passed to nu
            excluded = [
              "l"
              "ll"
            ];
          in
            lib.attrsets.filterAttrs
            (name: val: val != null && ! (builtins.elem name excluded))
            (osConfig.environment.shellAliases
              // config.home.shellAliases
              // {
                cal = "^cal";
              });
        };

        carapace = {
          enable = true;
          enableNushellIntegration = true;
        };
        zoxide = {
          enable = true;
          enableNushellIntegration = true;
        };
        direnv = {
          enable = true;
          enableNushellIntegration = true;
          nix-direnv.enable = true;
          config = {
            global = {
              warn_timeout = "-1s";
              hide_env_diff = true;
            };
          };
        };
      };

      home.file.".config/nushell/env.json".text =
        builtins.toJSON osConfig.environment.variables;
    }
    (lib.mkIf (builtins.elem pkgs.system lib.platforms.linux) {
      services = {
        pueue = {
          enable = true;
          settings = {
            daemon = {
              default_parallel_tasks = 65536;
            };
            shared = {
              use_unix_socket = true;
            };
          };
        };
      };
      home.shellAliases = {
        "bk" = "pueue";
      };
    })
  ];
}
