{
  config,
  pkgs,
  lib,
  nixpkgs,
  ...
}: {
  services = {
    mullvad-vpn = {
      enable = true;
      enableExcludeWrapper = false;
      package = pkgs.mullvad-vpn;
    };
    resolved.enable = true;
  };
}
