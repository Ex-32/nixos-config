{ inputs, config, pkgs, lib, nixpkgs, ... }:

{ 
  imports = [ inputs.impermanence.nixosModule ];
  
  environment.persistence."/persist" = {
    directories = [
      "/etc/nixos"
      "/var/lib/nixos"
      "/var/log"
    ] ++ (if config.networking.networkmanager.enable then 
      [ "/etc/NetworkManager/system-connections" ] else [])
      ++ (if config.hardware.bluetooth.enable then
      [ "/var/lib/bluetooth" ] else []);

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
