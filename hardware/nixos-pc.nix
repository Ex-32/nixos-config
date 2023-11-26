# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ config, lib, pkgs, modulesPath, ... }:

{
  imports =
    [ (modulesPath + "/installer/scan/not-detected.nix")
    ];

  boot.kernelParams = [
    "vsyscall=none"
    "consoleblank=60"
  ];

  boot.initrd.availableKernelModules = [ "nvme" "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-amd" ];
  boot.extraModulePackages = [ ];
  boot.swraid.enable = false;

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

  fileSystems."/nix" =
    { device = "/dev/disk/by-uuid/d043f002-e755-4a49-9316-58580bf9ec0a";
      fsType = "btrfs";
      options = [
        "compress=zstd"
        "subvol=@nix"
      ];
    };

  fileSystems."/var/log" =
    { device = "/dev/disk/by-uuid/d043f002-e755-4a49-9316-58580bf9ec0a";
      fsType = "btrfs";
      options = [
        "compress=zstd"
        "subvol=@nix-logs" 
      ];
    };

  fileSystems."/home" =
    { device = "/dev/disk/by-uuid/d043f002-e755-4a49-9316-58580bf9ec0a";
      fsType = "btrfs";
      options = [
        "compress=zstd"
        "subvol=@nix-home" 
      ];
    };
 
  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/d043f002-e755-4a49-9316-58580bf9ec0a";
      fsType = "btrfs";
      options = [ 
        "compress=zstd"
        "subvol=@nix-boot" 
      ];
    };

  fileSystems."/boot/efi" =
    { device = "/dev/disk/by-uuid/6347-5502";
      fsType = "vfat";
      options = [
        "noatime"
        "nosuid"
        "nodev"
        "noexec"
      ];
    };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/f18767e1-f510-42e1-b2fb-8ea4ffd21329"; }
    ];

  # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
  # (the default) this is the recommended approach. When using systemd-networkd it's
  # still possible to use this option, but it's recommended to use it in conjunction
  # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
  networking.useDHCP = lib.mkDefault true;
  # networking.interfaces.enp9s0.useDHCP = lib.mkDefault true;
  # networking.interfaces.wlp8s0.useDHCP = lib.mkDefault true;

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
