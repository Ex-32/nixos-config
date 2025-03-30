{
  config,
  pkgs,
  lib,
  nixpkgs,
  ...
}: {
  services.openssh = {
    enable = true;
    startWhenNeeded = true;

    # i'm dumb not stupid
    settings.PermitRootLogin = "no";
  };
}
