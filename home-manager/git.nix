{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
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
    settings = {
      user = {
        name = "Jenna Fligor";
        email = "jenna@fligor.net";
      };
      init.defaultBranch = "main";
      safe.directory = [
        "/etc/nixos"
        "/etc/nixos/.git"
      ];
    };
  };
}
