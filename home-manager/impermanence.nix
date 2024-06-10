{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  imports = [inputs.impermanence.nixosModules.home-manager.impermanence];

  home.persistence."/persist/safe/home/${config.home.username}" = {
    directories = [
      ".local/state"

      ".local/share/direnv"
      ".local/share/doom"
      ".local/share/gnupg"
      ".local/share/keyrings"
      ".local/share/nheko"
      ".local/share/nvim"
      ".local/share/wine"
      ".local/share/zoxide"

      ".config/1Password"
      ".config/RawTherapee"
      ".config/Signal"
      ".config/Slack"
      ".config/discord"
      ".config/htop"
      ".config/keepassxc"
      ".config/nheko"
      ".config/obsidian"
      ".config/spotify"

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
