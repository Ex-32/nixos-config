{
  inputs,
  config,
  pkgs,
  lib,
  nixpkgs,
  ...
}: {
  imports = [inputs.impermanence.nixosModule];

  # this defines what files/directories should be bind-mounted from the
  # persistant storage at /persist to the system, any files/directories *not*
  # either in this list, or else mounted from another partition/subvolume (like
  # /home or /nix) will be created on a tmpfs and be lost on poweroff
  environment.persistence."/persist/safe/system" = {
    directories = let
      mergeIf = predicate: value:
        if predicate
        then value
        else [];
    in
      [
        "/etc/nixos"
        "/var/lib/nixos"
        "/var/log"
        # below we use the `config` variable to introspect the state of this
        # config as defined by other parts of the flake, this allows conditional
        # enabling of persistent storage at certain locations when specific
        # features are enabled such as NetworkManagers connection data store and
        # libvirtd's VM image store
      ]
      ++ (mergeIf config.networking.networkmanager.enable ["/etc/NetworkManager/system-connections"])
      ++ (
        mergeIf (config.networking.wireless.iwd.enable
          || config.networking.networkmanager.wifi.backend == "iwd") ["/var/lib/iwd"]
      )
      ++ (mergeIf config.hardware.bluetooth.enable ["/var/lib/bluetooth"])
      ++ (mergeIf config.virtualisation.libvirtd.enable ["/var/lib/libvirt"])
      ++ (mergeIf config.services.fprintd.enable ["/var/lib/fprint"]);

    files = [
      "/etc/machine-id"
    ];
  };

  fileSystems."/" = {
    device = "none";
    fsType = "tmpfs";
    options = [
      "size=100%"
      "huge=within_size"
      "mode=755"
      "noatime"
    ];
  };
}
