{
  config,
  osConfig,
  pkgs,
  lib,
  inputs,
  ...
}: let
  symlink = path: {
    directory = path;
    method = "symlink";
  };
in {
  imports = [inputs.impermanence.nixosModules.home-manager.impermanence];

  home.persistence."/persist/safe/home/${config.home.username}" = {
    directories = [
      ".local/state"

      (symlink ".local/share/direnv")
      (symlink ".local/share/doom")
      (symlink ".local/share/gnupg")
      (symlink ".local/share/keyrings")
      (symlink ".local/share/nheko")
      (symlink ".local/share/nvim")
      (symlink ".local/share/wine")
      (symlink ".local/share/xonsh")
      (symlink ".local/share/zoxide")

      (symlink ".config/1Password")
      (symlink ".config/RawTherapee")
      (symlink ".config/Signal")
      (symlink ".config/Slack")
      (symlink ".config/discord")
      (symlink ".config/htop")
      (symlink ".config/keepassxc")
      (symlink ".config/nheko")
      (symlink ".config/nvim/spell")
      (symlink ".config/obsidian")
      (symlink ".config/spotify")

      ".mozilla"
      ".ssh"
      "documents"
      "src"
    ];
  };

  home.persistence."/persist/volatile/cache/${config.home.username}" = {
    directories = [
      ".cache"
    ];
  };

  home.persistence."/persist/volatile/games/${config.home.username}" = {
    directories = [
      (symlink ".local/share/Steam")
      (symlink ".local/share/epic-games")
      (symlink ".local/share/vulkan")
      (symlink ".steam")
    ];
  };
}
