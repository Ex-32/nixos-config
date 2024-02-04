{ config, pkgs, lib, nixpkgs, ... }:

{
  console = {
    # this performs tty configuration in the initrd, enabled mostly for
    # aesthetic purposes (so the font gets set asap)
    earlySetup = true;

    # this installs and sets the tty consolefont to the spleen font with a size
    # of 16x32, this will look very big on a low-res display, but looks very
    # nice on most modern displays
    packages = with pkgs; [ spleen ];
    font = "spleen-16x32";

    # this sets the tty colors to colors from the catppuccin theme, this is
    # done through some exceptionally long kernel arguments, so commenting out
    # this block may make the kernel arguments more readable for debugging,
    # with only aesthetic consequences
    colors = [
      "11111b" # crust
      "f38ba8" # red
      "a6e3a1" # green
      "fab387" # peach
      "89b4fa" # blue
      "cba6f7" # mauve
      "89dceb" # sky
      "a6adc8" # subtext 0
      "1e1e2e" # base
      "f5c2e7" # pink
      "a6e3a1" # green (again...)
      "f9e2af" # yellow
      "74c7ec" # sapphire
      "b3befe" # lavender
      "94e2d5" # teal
      "cdd6f4" # text
    ];
  };

  # this disables the tty login help line about the nixos manual for a
  # cleaner login prompt
  services.getty.helpLine = lib.mkForce "";
}
