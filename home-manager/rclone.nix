{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  sops.secrets."homelab/copyparty" = {};

  programs.rclone = {
    enable = true;

    remotes = {
      copyparty = {
        config = {
          type = "webdav";
          url = "https://memory-hole.tail3782b9.ts.net:4403/";
          vendor = "owncloud";
          user = "admin";
          pacer_min_sleep = "10ms";
        };

        secrets.pass = config.sops.secrets."homelab/copyparty".path;

        mounts."/" = {
          enable = true;
          autoMount = true;
          mountPoint = "${config.home.homeDirectory}/copyparty";
          logLevel = "INFO";
          options = {
            vfs-cache-mode = "writes";
            vfs-cache-max-age = "5s";
            attr-timeout = "5s";
            dir-cache-time = "5s";
          };
        };
      };
    };
  };
}
