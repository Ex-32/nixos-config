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
    options = let
      # this line prevents hanging on network split
      automount_opts = "x-systemd.automount,noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=5s";
    in ["${automount_opts},credentials=/etc/nixos/secrets/smb/homelab,file_mode=0660,dir_mode=0770,gid=${toString config.users.groups.wheel.gid}"];
  };
}
