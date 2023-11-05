{ config, pkgs, lib, nixpkgs, ... }:

{
  services.openssh = {
    enable = true;
    ports = [ 3932 ];
    startWhenNeeded = true;
    settings.PermitRootLogin = "no";
  };
}
