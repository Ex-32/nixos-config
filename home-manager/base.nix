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

    home.stateVersion = osConfig.system.stateVersion;
  };
}
