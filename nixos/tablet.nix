{
  config,
  pkgs,
  lib,
  nixpkgs,
  ...
}: {
  hardware.opentabletdriver = {
    enable = true;
    daemon.enable = true;
    package = pkgs.opentabletdriver;
  };

  environment.systemPackages = [config.hardware.opentabletdriver.package];
}
