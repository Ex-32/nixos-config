{ config, pkgs, lib, inputs, ... }:

{
  home.packages = with pkgs; [
    lazygit
  ];

  programs.gh = {
    enable = true;
    gitCredentialHelper.enable = true;
    # HACK: this is a *very* hacky fix for HM issue #4744
    settings = {
      version = 1;
    };
  };

  programs.git = {
    enable = true;
    userName = "Ex-32";
    userEmail = "jenna@fligor.net";
    extraConfig = {
      init.defaultBranch = "main";
      safe.directory = "/etc/nixos";
    };
  };
}
