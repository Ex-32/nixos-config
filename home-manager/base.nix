{
  config,
  pkgs,
  lib,
  inputs,
  osConfig,
  ...
}: {
  # Pending https://github.com/NixOS/nixpkgs/issues/55674
  options.allowedUnfree = lib.mkOption {
    type = lib.types.listOf lib.types.str;
    default = [];
  };

  config = {
    programs.home-manager.enable = true;

    # there's probably a more correct way to do this, but since $MANPATH gets set
    # anyway, there's no need to create this file, and $HOME clutter is my
    # personal enemy
    home.file.".manpath".enable = false;

    # FIXME: work on bot nixos and nix Darwin
    # stateVersion mismatches between nixpkgs and home-manager are... *bad*
    home.stateVersion = "24.11"; # osConfig.system.stateVersion; 
  };
}
