{ inputs, config, pkgs, lib, nixpkgs, ... }:

{ 
  imports = [ inputs.impermanence.nixosModule ];

  # this defines what files/directories should be bind-mounted from the
  # persistant storage at /persist to the system, any files/directories *not*
  # either in this list, or else mounted from another partition/subvolume (like
  # /home or /nix) will be created on a tmpfs and be lost on poweroff
  environment.persistence."/persist" = {
    directories = [
      "/etc/nixos"
      "/var/lib/nixos"
      "/var/log"
      # below we use the `config` variable to introspect the state of this
      # config as defined by other parts of the flake, this allows conditional
      # enabling of persistent storage at certain locations when specific
      # features are enabled such as NetworkManagers connection data store and
      # libvirtd's VM image store
    ] ++ (if config.networking.networkmanager.enable then 
      [ "/etc/NetworkManager/system-connections" ] else [])
      ++ (if config.hardware.bluetooth.enable then
      [ "/var/lib/bluetooth" ] else [])
      ++ (if config.virtualisation.libvirtd.enable then
      [ "/var/lib/libvirt" ] else []);

    files = [
      "/etc/machine-id"
    ];
  };

  # this needs to be set even though / is a tmpfs because /tmp can't be noexec
  # or nix will throw cryptic build errors if has to build any derivation that
  # involves executing a binary that was made in-situ rather than from a store
  # path
  boot.tmp.useTmpfs = true;

  # since the bulk of the system is stored in the nix store, there's actually
  # very little in / so a large allocation isn't needed, in fact, my usage
  # rarely reaches above a couple megabytes
  fileSystems."/" = {
    device = "none";
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
