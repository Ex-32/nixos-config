{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  imports = [inputs.impermanence.nixosModules.home-manager.impermanence];

  home.persistence."/persist/safe/home/${config.home.username}" = {
    directories = let
      symlink = path: {
        directory = path;
        method = "symlink";
      };
    in [
      ".local/state"

      (symlink ".local/share/direnv")
      (symlink ".local/share/doom")
      (symlink ".local/share/gnupg")
      (symlink ".local/share/keyrings")
      (symlink ".local/share/nheko")
      (symlink ".local/share/nvim")
      (symlink ".local/share/wine")
      (symlink ".local/share/zoxide")

      (symlink ".config/1Password")
      (symlink ".config/RawTherapee")
      (symlink ".config/Signal")
      (symlink ".config/Slack")
      (symlink ".config/discord")
      (symlink ".config/htop")
      (symlink ".config/keepassxc")
      (symlink ".config/nheko")
      (symlink ".config/obsidian")
      (symlink ".config/spotify")

      ".mozilla"
      ".ssh"
      "documents"
      "src"
    ];

    files = [
      ".local/share/fish/fish_history"
    ];
  };

  home.persistence."/persist/volatile/cache/${config.home.username}" = {
    directories = [
      ".cache"
    ];
  };
}
