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
  home.file.".config/gh/hosts.yml".text = ''
    github.com:
        git_protocol: https
        users:
            Ex-32:
        user: Ex-32
  '';

  programs.git = {
    enable = true;
    userName = "Jenna Fligor";
    userEmail = "jenna@fligor.net";
    extraConfig = {
      init.defaultBranch = "main";
      safe.directory = [
        "/etc/nixos"
        "/etc/nixos/.git"
      ];
    };
  };
}
