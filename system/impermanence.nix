{ config, pkgs, lib, nixpkgs, ... }:

{
  environment.persistence."/persist" = {
    directories = [
      "/etc/nixos"
      "/var/lib/nixos"
      "/var/log"
      # "/var/lib/bluetooth" # add this to system/bluetooth.nix if/when it exists
    ];
    files = [
      "/etc/machine-id"
    ];
  };

  # this needs to be set even though / is a tmpfs because /tmp can't be noexec
  boot.tmp.useTmpfs = true;

  fileSystems."/" =
    { device = "none";
      fsType = "tmpfs";
      options = [
          "size=1G"
          "mode=755"
          "noatime"
          "nosuid"
          "nodev"
          "noexec"
      ];
    };
}
