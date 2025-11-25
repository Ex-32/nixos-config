{
  config,
  pkgs,
  lib,
  nixpkgs,
  ...
}: {
  console = {
    # this performs tty configuration in the initrd, enabled mostly for
    # aesthetic purposes (so the font gets set asap)
    earlySetup = true;

    # this installs and sets the tty consolefont to the spleen font with a size
    # of 16x32, this will look very big on a low-res display, but looks very
    # nice on most modern displays
    packages = with pkgs; [spleen];
    font = "spleen-12x24";
  };

  # this disables the tty login help line about the nixos manual for a
  # cleaner login prompt
  services.getty.helpLine = lib.mkForce "";
}
