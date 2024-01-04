{ config, pkgs, lib, nixpkgs, ... }:

{
  # graphical enviroments tend to get lost without openGL, this tells nixos to
  # figure out what kind of hardware it's on an install the appropriate driver
  hardware.opengl.enable = true;

  # this installs a default set of fonts (opensans and the like) so that
  # there's a decent serif, sans-serif, and monospace font installed for
  # well... everything
  fonts.enableDefaultPackages = true;
  programs.dconf.enable = true;
  services.udisks2.enable = true;
}
