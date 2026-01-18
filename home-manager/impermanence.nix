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

  installed-pkgs =
    builtins.map lib.getName
    (config.home.packages ++ osConfig.environment.systemPackages);

  listify = x:
    if builtins.isList x
    then x
    else [x];

  if-set = cond: path: lib.lists.optionals cond (listify path);

  if-pkg = pkgs: path:
    lib.lists.optionals
    (builtins.any
      (pkg:
        builtins.elem (
          if lib.isDerivation pkg
          then lib.getName pkg
          else pkg
        )
        installed-pkgs)
      (listify pkgs))
    (listify path);
in {
  config = lib.mkMerge [
    {
      home.persistence."/persist/safe" = {
        directories =
          [
            ".local/state"

            ".local/share/com.github.johnfactotum.Foliate"
            ".local/share/containers"
            ".local/share/direnv"
            ".local/share/doom"
            ".local/share/gnupg"
            ".local/share/keyrings"
            ".local/share/nheko"
            ".local/share/nvim"
            ".local/share/nyxt"
            ".local/share/qBittorrent"
            ".local/share/wine"
            ".local/share/xonsh"
            ".local/share/zoxide"

            ".config/1Password"
            ".config/Bitwarden"
            ".config/DeltaChat"
            ".config/Element"
            ".config/RawTherapee"
            ".config/Signal"
            ".config/Slack"
            ".config/anytype"
            ".config/discord"
            ".config/emacs"
            ".config/htop"
            ".config/nvim/spell"
            ".config/qBittorrent"
            ".config/sops"
            ".config/spotify"
            ".config/steamtinkerlaunch"

            ".mozilla"
            ".ssh"
            "documents"
            "src"
          ]
          ++ (if-set config.services.remmina.enable [
            ".config/remmina"
            ".local/share/remmina"
          ])
          ++ (if-set config.programs.zsh.enable ".local/share/zsh")
          ++ (if-set config.services.kdeconnect.enable ".config/kdeconnect")
          ++ (if-set osConfig.hardware.opentabletdriver.daemon.enable ".config/OpenTabletDriver");
        files = [
          ".config/gh/hosts.yml"
          ".local/share/fish/fish_history"
        ];
      };

      home.persistence."/persist/volatile/cache" = {
        directories = [
          ".cache"
        ];
      };
    }
    (lib.mkIf (builtins.hasAttr "/persist/volatile/games" osConfig.fileSystems) {
      home.persistence."/persist/volatile/games" = {
        directories =
          (if-set osConfig.programs.steam.enable [
            ".config/StardewValley"
            ".config/Stardrop"
            ".config/unity3d"
            ".factorio"
            ".local/share/EXAPUNKS"
            ".local/share/Opus Magnum"
            ".local/share/SHENZHEN IO"
            ".local/share/Steam"
            ".local/share/TIS-100"
            ".local/share/YourOnlyMoveIsHUSTLE"
            ".local/share/Zachtronics Industries"
            ".local/share/vulkan"
            ".steam"
          ])
          ++ (if-set config.local.lutris.enable [
            ".local/share/epic-games"
          ])
          ++ (if-pkg pkgs.prismlauncher ".local/share/PrismLauncher")
          ++ (if-pkg pkgs.endless-sky ".local/share/endless-sky")
          ++ (if-pkg pkgs.superTuxKart ".local/share/supertuxkart");
      };
    })
    (lib.mkIf (builtins.hasAttr "/persist/volatile/vm" osConfig.fileSystems) {
      home.persistence."/persist/volatile/vm" = {
        directories = if-pkg pkgs.winboat [
          ".winboat"
          ".config/winboat"
          ".local/share/winboat"
        ];
      };
    })
  ];
}
