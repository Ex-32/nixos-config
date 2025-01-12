{
  inputs,
  config,
  pkgs,
  lib,
  nixpkgs,
  ...
}: let
  optional = lib.lists.optional;
  optionals = lib.lists.optionals;
in {
  imports = [inputs.impermanence.nixosModule];

  # this doesn't do anything for the system-wide impermanence, but it allows
  # home-manager impermanence to set allowOther for fuse bind-mounts, which
  # prevents weird errors when trying to use sudo on persistence mounts.
  programs.fuse.userAllowOther = true;

  # this defines what files/directories should be bind-mounted from the
  # persistant storage at /persist to the system, any files/directories *not*
  # either in this list, or else mounted from another partition/subvolume (like
  # /home or /nix) will be created on a tmpfs and be lost on poweroff
  environment.persistence."/persist/safe/system" = {
    directories =
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
      ++ (optional config.networking.networkmanager.enable "/etc/NetworkManager/system-connections")
      ++ (
        optional (config.networking.wireless.iwd.enable
          || config.networking.networkmanager.wifi.backend == "iwd") "/var/lib/iwd"
      )
      ++ (optional config.hardware.bluetooth.enable "/var/lib/bluetooth")
      ++ (optional config.virtualisation.libvirtd.enable "/var/lib/libvirt")
      ++ (optional config.services.fprintd.enable "/var/lib/fprint")
      ++ (optional config.services.tailscale.enable "/var/lib/tailscale")
      ++ (optional config.services.mullvad-vpn.enable "/etc/mullvad-vpn");

    files = [
      "/etc/machine-id"
    ];
  };

  # don't fuck with /etc/machine-id since we're handling that
  systemd.services."systemd-machine-id-commit".enable = false;

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
