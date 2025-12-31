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
  imports = [inputs.impermanence.nixosModules.home-manager.impermanence];

  config = lib.mkMerge [
    {
      home.persistence."/persist/safe/home/${config.home.username}" = {
        allowOther = osConfig.programs.fuse.userAllowOther;
        directories =
          [
            ".local/state"

            (symlink ".local/share/com.github.johnfactotum.Foliate")
            (symlink ".local/share/containers")
            (symlink ".local/share/direnv")
            (symlink ".local/share/doom")
            (symlink ".local/share/gnupg")
            (symlink ".local/share/keyrings")
            (symlink ".local/share/nheko")
            (symlink ".local/share/nvim")
            (symlink ".local/share/nyxt")
            (symlink ".local/share/qBittorrent")
            (symlink ".local/share/wine")
            (symlink ".local/share/xonsh")
            (symlink ".local/share/zoxide")

            (symlink ".config/1Password")
            (symlink ".config/Bitwarden")
            (symlink ".config/DeltaChat")
            (symlink ".config/Element")
            (symlink ".config/RawTherapee")
            (symlink ".config/Signal")
            (symlink ".config/Slack")
            (symlink ".config/anytype")
            (symlink ".config/discord")
            (symlink ".config/emacs")
            (symlink ".config/htop")
            (symlink ".config/nvim/spell")
            (symlink ".config/qBittorrent")
            (symlink ".config/sops")
            (symlink ".config/spotify")
            (symlink ".config/steamtinkerlaunch")

            ".mozilla"
            ".ssh"
            "documents"
            "src"
          ]
          ++ (if-set config.services.remmina.enable [
            (symlink ".config/remmina")
            (symlink ".local/share/remmina")
          ])
          ++ (if-set config.programs.zsh.enable (symlink ".local/share/zsh"))
          ++ (if-set config.services.kdeconnect.enable (symlink ".config/kdeconnect"))
          ++ (if-set osConfig.hardware.opentabletdriver.daemon.enable (symlink ".config/OpenTabletDriver"));
        files = [
          ".config/gh/hosts.yml"
          ".local/share/fish/fish_history"
        ];
      };

      home.persistence."/persist/volatile/cache/${config.home.username}" = {
        allowOther = osConfig.programs.fuse.userAllowOther;
        directories = [
          ".cache"
        ];
      };
    }
    (lib.mkIf (builtins.hasAttr "/persist/volatile/games" osConfig.fileSystems) {
      home.persistence."/persist/volatile/games/${config.home.username}" = {
        allowOther = osConfig.programs.fuse.userAllowOther;
        directories =
          (if-set osConfig.programs.steam.enable [
            ".factorio"
            (symlink ".config/StardewValley")
            (symlink ".config/Stardrop")
            (symlink ".local/share/EXAPUNKS")
            (symlink ".local/share/NexusMods.App")
            (symlink ".local/share/Opus Magnum")
            (symlink ".local/share/Steam")
            (symlink ".local/share/Zachtronics Industries")
            (symlink ".local/share/vulkan")
            (symlink ".steam")
          ])
          ++ (if-set config.local.lutris.enable [
            (symlink ".local/share/epic-games")
          ])
          ++ (if-pkg pkgs.prismlauncher (symlink ".local/share/PrismLauncher"))
          ++ (if-pkg pkgs.endless-sky (symlink ".local/share/endless-sky"))
          ++ (if-pkg pkgs.superTuxKart (symlink ".local/share/supertuxkart"));
      };
    })
    (lib.mkIf (builtins.hasAttr "/persist/volatile/vm" osConfig.fileSystems) {
      home.persistence."/persist/volatile/vm/home/${config.home.username}" = {
        allowOther = osConfig.programs.fuse.userAllowOther;
        directories = if-pkg pkgs.winboat [
          (symlink ".winboat")
          (symlink ".config/winboat")
          (symlink ".local/share/winboat")
        ];
      };
    })
  ];
}
