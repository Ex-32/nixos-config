{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  imports = [
    ./nerdfont.nix
  ];
  programs.yazi = {
    enable = true;
    settings = {
      manager = {
        scrolloff = 3;
        show_hidden = true;
        sort_by = "natural";
      };
    };
  };
  home.file = {
    ".config/yazi/theme.toml".source =
      pkgs.fetchFromGitHub {
        owner = "catppuccin";
        repo = "yazi";
        rev = "0846aed69b2a62d29c98e100af0cf55ca729723d";
        sha256 = "sha256-2T41qWMe++3Qxl9veRNHMeRI3eU4+LAueKTss02gYNk=";
      }
      + "/themes/mocha.toml";
    ".config/yazi/Catppuccin-mocha.tmTheme".source =
      pkgs.fetchFromGitHub {
        owner = "catppuccin";
        repo = "bat";
        rev = "b19bea35a85a32294ac4732cad5b0dc6495bed32";
        hash = "sha256-POoW2sEM6jiymbb+W/9DKIjDM1Buu1HAmrNP0yC2JPg=";
      }
      + "/themes/Catppuccin Mocha.tmTheme";
  };
}
