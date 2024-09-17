{
  config,
  osConfig,
  pkgs,
  lib,
  inputs,
  ...
}: {
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
        osConfig.environment.shellAliases;
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
