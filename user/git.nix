{ config, pkgs, lib, inputs, ... }:

{
  home.packages = with pkgs; [
    lazygit
  ];

  programs.gh = {
    enable = true;
    gitCredentialHelper = {
      enable = true;
      hosts = [
        "https://github.com"
        "https://gist.github.com"
      ];
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
