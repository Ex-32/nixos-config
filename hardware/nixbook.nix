{ config, lib, pkgs, modulesPath, ... }:

{
  imports =
    [ (modulesPath + "/installer/scan/not-detected.nix")
    ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "thunderbolt" "nvme" "usb_storage" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/85a0ca7d-1dee-42aa-a493-a36f748ecc98";
      fsType = "btrfs";
      options = [
      	"subvol=@nix-root"
      	"compress=zstd"
      ];
    };

  boot.initrd.luks.devices."cryptdisk".device = "/dev/disk/by-uuid/7dbd4605-430f-4ea5-b8e3-14f7a4ee82a7";

  fileSystems."/home" =
    { device = "/dev/disk/by-uuid/85a0ca7d-1dee-42aa-a493-a36f748ecc98";
      fsType = "btrfs";
      options = [
      	"subvol=@home"
      	"compress=zstd"
      ];
    };

  fileSystems."/home/jenna/games" =
    { device = "/dev/disk/by-uuid/85a0ca7d-1dee-42aa-a493-a36f748ecc98";
      fsType = "btrfs";
      options = [
      	"subvol=@games"
      	"compress=zstd"
      ];
    };

  fileSystems."/var/log" =
    { device = "/dev/disk/by-uuid/85a0ca7d-1dee-42aa-a493-a36f748ecc98";
      fsType = "btrfs";
      options = [
      	"subvol=@nix-logs" 
      	"compress=zstd"
      ];
    };

  fileSystems."/nix" =
    { device = "/dev/disk/by-uuid/85a0ca7d-1dee-42aa-a493-a36f748ecc98";
      fsType = "btrfs";
      options = [
      	"subvol=@nix-store"
      	"compress=zstd"
      ];
    };

  fileSystems."/mnt/fsroot" =
    { device = "/dev/disk/by-uuid/85a0ca7d-1dee-42aa-a493-a36f748ecc98";
      fsType = "btrfs";
      options = [
      	"subvol=/"
      	"compress=zstd"
      ];
    };

  fileSystems."/boot/efi" =
    { device = "/dev/disk/by-uuid/18A2-E6E3";
      fsType = "vfat";
    };



  swapDevices = [ ];

  # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
  # (the default) this is the recommended approach. When using systemd-networkd it's
  # still possible to use this option, but it's recommended to use it in conjunction
  # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
  networking.useDHCP = lib.mkDefault true;
  # networking.interfaces.wlp170s0.useDHCP = lib.mkDefault true;

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
