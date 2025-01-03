{
  config,
  pkgs,
  lib,
  nixpkgs,
  ...
}: {
  services.tailscale.enable = true;
}
