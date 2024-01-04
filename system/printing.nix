{ config, pkgs, lib, nixpkgs, ... }:

{
  services.printing = {
    enable = true;
    startWhenNeeded = true;

    # printer drivers *must* be put here, just adding them to
    # environment.systemPackages doesn't make them available to to CUPS
    drivers = with pkgs; [
      brlaser
      cnijfilter2
      gutenprint
      gutenprintBin
      hplipWithPlugin
    ];
  };

  # this is needed for wifi printing and automatic printer discovery to work
  services.avahi = {
    enable = true;
    nssmdns4 = true;
    openFirewall = true;
  };
}
