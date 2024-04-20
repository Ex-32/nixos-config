{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: let
  spicePkgs = inputs.spicetify-nix.packages.${pkgs.system}.default;
in {
  imports = [inputs.spicetify-nix.homeManagerModule];

  allowedUnfree = ["spotify"];

  programs.spicetify = {
    enable = true;
    theme = spicePkgs.themes.catppuccin;
    colorScheme = "mocha";

    enabledExtensions = with spicePkgs.extensions; [
      autoSkipVideo
      keyboardShortcut
      loopyLoop
      shuffle
      trashbin
      playlistIntersection
      skipStats
    ];
  };
}
