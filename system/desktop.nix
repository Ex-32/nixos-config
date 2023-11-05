{ config, pkgs, lib, nixpkgs, ... }:

{
  hardware.opengl.enable = true;
  fonts.enableDefaultPackages = true;
  programs.dconf.enable = true;
  services.udisks2.enable = true;
}
