{
  config,
  pkgs,
  lib,
  nixpkgs,
  ...
}: {
  services = {
    tailscale = {
      enable = true;
      openFirewall = true;
    };
    resolved.enable = true;
  };
}
