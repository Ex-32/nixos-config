{ config, pkgs, lib, nixpkgs, ... }:

{
  services.printing = {
    enable = true;
    startWhenNeeded = true;
    drivers = with pkgs; [
      brlaser
      cnijfilter2
      gutenprint
      gutenprintBin
      hplip
      hplipWithPlugin
    ];
  };
  services.avahi = {
    enable = true;
    nssmdns = true;
    openFirewall = true;
  };
}
