{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  home.persistence."/persist/safe${config.home.homeDirectory}" = {
    directories = [
      {
        directory = ".local/share/gnupg";
        mode = "0700";
      }
      {
        directory = ".ssh";
        mode = "0700";
      }
      {
        directory = ".local/share/keyrings";
        mode = "0700";
      }

      ".local/state"

      ".local/share/nvim"
      ".local/share/direnv"
      ".local/share/doom"
      ".local/share/wine"

      ".config/1Password"
      ".config/discord"
      ".config/keepassxc"
      ".config/nheko"
      ".config/obsidian"
      ".config/Signal"
      ".config/Slack"
      ".config/spotify"
      ".config/RawTherapee"

      ".mozilla"
      "documents"
      "src"
    ];

    files = [
      ".local/share/fish/fish_history"
    ];
  };

  home.persistence."/persist/cache${config.home.homeDirectory}" = {
    directories = [
      ".cache"
    ];
  };
}
