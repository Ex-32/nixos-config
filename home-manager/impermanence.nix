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
  optional = lib.lists.optional;
  optionals = lib.lists.optionals;
  removeHome = str: lib.strings.removePrefix config.home.homeDirectory str;
in {
  imports = [inputs.impermanence.nixosModules.home-manager.impermanence];

  home.persistence."/persist/safe/home/${config.home.username}" = {
    allowOther = osConfig.programs.fuse.userAllowOther;
    directories =
      [
        ".local/state"

        (symlink ".local/share/containers")
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
        (symlink ".config/Bitwarden")
        (symlink ".config/RawTherapee")
        (symlink ".config/Signal")
        (symlink ".config/Slack")
        (symlink ".config/discord")
        (symlink ".config/htop")
        (symlink ".config/nheko")
        (symlink ".config/nushell/history.sqlite3")
        (symlink ".config/nvim/spell")
        (symlink ".config/spotify")
        (symlink ".config/obsidian")

        ".mozilla"
        ".ssh"
        "documents"
        "src"
      ]
      ++ (optional config.programs.zsh.enable (symlink ".local/share/zsh"));
    files = [
      ".config/gh/hosts.yml"
    ];
  };

  home.persistence."/persist/volatile/cache/${config.home.username}" = {
    allowOther = osConfig.programs.fuse.userAllowOther;
    directories = [
      ".cache"
    ];
  };

  home.persistence."/persist/volatile/games/${config.home.username}" = {
    allowOther = osConfig.programs.fuse.userAllowOther;
    directories =
      (optionals osConfig.programs.steam.enable [
        (symlink ".factorio")
        (symlink ".local/share/Steam")
        (symlink ".local/share/vulkan")
        (symlink ".steam")
      ])
      ++ (optionals config.local.lutris.enable [
        (symlink ".local/share/epic-games")
      ])
      ++ (optionals (
          builtins.elem pkgs.prismlauncher
          (config.home.packages ++ osConfig.environment.systemPackages)
        ) [
          (symlink ".local/share/PrismLauncher")
        ]);
  };
}
