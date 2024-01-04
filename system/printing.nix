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
      hplipWithPlugin
    ];
  };
  services.avahi = {
    enable = true;
    nssmdns4 = true;
    openFirewall = true;
  };
}
