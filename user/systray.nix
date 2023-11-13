{ config, pkgs, lib, inputs, ... }:

{
  services.udiskie = {
    enable = true;
    automount = false;
  };
  services.network-manager-applet.enable = true;
}
