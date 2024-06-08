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

  programs.nushell = {
    enable = true;
    configFile.source = ../config/nushell/config.nu;
    envFile.source = ../config/nushell/env.nu;
    shellAliases = {
      sc = "sudo systemctl";
      scu = "systemctl --user";
      jc = "journalctl";
      bk = "pueue";

      nor = "nh os switch -a /etc/nixos";
      py = "nix shell nixpkgs#python3 --command python3";
    };
  };

  # external program completion hints
  programs.carapace = {
    enable = true;
    enableNushellIntegration = true;
  };

  # nushell doesn't native background processes, but we can fake it
  services.pueue = {
    enable = true;
    settings = {
      daemon = {
        default_parallel_tasks = 65535;
      };
    };
  };

  programs.zoxide = {
    enable = true;
    enableNushellIntegration = true;
  };

  programs.direnv = {
    enable = true;
    enableNushellIntegration = true;
    nix-direnv.enable = true;
  };
}
