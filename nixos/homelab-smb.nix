{
  config,
  pkgs,
  lib,
  nixpkgs,
  ...
}: {
  environment.systemPackages = [pkgs.cifs-utils];
  fileSystems."/mnt/homelab" = {
    device = "//100.100.1.164/main";
    fsType = "cifs";
    options = [
      # prevent hanging on network failure
      "x-systemd.automount"
      "noauto"
      "x-systemd.idle-timeout=60"
      "x-systemd.device-timeout=5s"
      "x-systemd.mount-timeout=5s"

      # mount config
      "credentials=${config.sops.secrets."homelab/smb".path}"
      "file_mode=0660"
      "dir_mode=0770"
      "gid=${toString config.users.groups.wheel.gid}"
    ];
  };
  sops.secrets."homelab/smb" = {};
}
