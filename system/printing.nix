{ config, pkgs, lib, nixpkgs, ... }:

{
  services.printing = {
    enable = true;
    # startWhenNeeded = true;
    # drivers = with pkgs; [
    #   gutenprint
    #   gutenprintBin
    #   hplip
    #   hplipWithPlugin
    #   brlaser
    # ];
  };
  services.avahi = {
    enable = true;
    nssmdns = true;
    openFirewall = true;
  };
}
