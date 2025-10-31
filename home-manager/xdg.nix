{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  home.packages = with pkgs; [
    xdg-utils
  ];

  xdg.userDirs = let
    home = config.home.homeDirectory;
  in {
    enable = true;
    createDirectories = true;
    desktop = "${home}/documents";
    documents = "${home}/documents";
    download = "${home}/downloads";
    music = "${home}/documents/music";
    pictures = "${home}/documents/pictures";
    publicShare = null;
    templates = null;
    videos = "${home}/documents/videos";
  };

  # many assorted things expect an xdg secrets implementation, and
  # gnome-keyring is the least bad option, so we enable it by default
  services.gnome-keyring.enable = lib.mkDefault true;

  # hack to make npm xdg compliant
  home.file.".config/npm/npmrc".text = ''
    prefix=''${XDG_DATA_HOME}/npm
    cache=''${XDG_CACHE_HOME}/npm
    init-module=''${XDG_CONFIG_HOME}/npm/config/npm-init.js
  '';
}
