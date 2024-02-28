{ config, pkgs, lib, inputs, osConfig, ... }:

{
  services.udiskie = {
    enable = true;
    automount = false;
  };
  services.network-manager-applet.enable = osConfig.networking.networkmanager.enable;
  services.blueman-applet.enable = osConfig.services.blueman.enable;
}
