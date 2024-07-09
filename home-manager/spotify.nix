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
    theme = spicePkgs.themes.catppuccin // {
      src = pkgs.fetchFromGitHub {
        owner = "catppuccin";
        repo = "spicetify";
        rev = "4294a61f54a044768c6e9db20e83c5b74da71091";
        hash = "sha256-OHsauoCjj99aoIbq78xQf1ehYtLpIcUde5DmZSJFCXI=";
      };
    };
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
