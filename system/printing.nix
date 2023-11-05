{ config, pkgs, lib, nixpkgs, ... }:

{
  services.printing = {
    enable = true;
    # startWhenNeeded = true;
  };
  services.avahi = {
    enable = true;
    nssmdns = true;
    openFirewall = true;
  };
}
