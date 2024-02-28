{
  config,
  pkgs,
  lib,
  nixpkgs,
  ...
}: {
  # graphical environments tend to get lost without OpenGL, this tells nixos to
  # figure out what kind of hardware it's on an install the appropriate driver
  hardware.opengl.enable = true;

  # this installs a default set of fonts (opensans and the like) so that
  # there's a decent serif, sans-serif, and monospace font installed for
  # well... everything
  fonts.enableDefaultPackages = true;

  # many gtk applications don't behave properly unless they can query dconf for
  # settings, so regardless of windowing environment it's good to have it
  # enabled
  programs.dconf.enable = true;

  # unprivileged access to disks is a pain in the ass without udisks so, so
  # it's always worth having in a graphical system where you'll be logged in as
  # an unprivileged user most of the time
  services.udisks2.enable = true;
}
