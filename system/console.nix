{ config, pkgs, lib, nixpkgs, ... }:

{
  console = {
    earlySetup = true;
    packages = with pkgs; [ spleen ];
    font = "spleen-16x32";
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

  services.getty.helpLine = lib.mkForce "";
}
