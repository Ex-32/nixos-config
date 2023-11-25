{ config, lib, pkgs, modulesPath, ... }:

{
  imports =
    [ (modulesPath + "/installer/scan/not-detected.nix")
    ];

  boot.initrd.availableKernelModules = [ "nvme" "xhci_pci" "ahci" "usbhid" "sd_mod" ];
  boot.initrd.kernelModules = [ "dm-snapshot" ];
  boot.kernelModules = [ "kvm-amd" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" = { 
    device = "/dev/disk/by-uuid/43bf7935-ca90-4d0d-8d60-3c19ca8d5584";
    fsType = "btrfs";
    options = [ 
      "subvol=/@"
      "compress-force=zstd:15"
    ];
  };

  boot.initrd.luks.devices."luks-d106ba24-da3a-4c5c-896f-734c430164ef".device = "/dev/disk/by-uuid/d106ba24-da3a-4c5c-896f-734c430164ef";

  fileSystems."/home" = {
    device = "/dev/disk/by-uuid/43bf7935-ca90-4d0d-8d60-3c19ca8d5584";
    fsType = "btrfs";
    options = [
      "subvol=/@home"
      "compress-force=zstd:15"
    ];
  };

  fileSystems."/nix" = { 
    device = "/dev/disk/by-uuid/43bf7935-ca90-4d0d-8d60-3c19ca8d5584";
    fsType = "btrfs";
    options = [
      "subvol=/@nix"
      "compress-force=zstd:15"
    ];
  };

  fileSystems."/boot" = { 
    device = "/dev/disk/by-uuid/BDD7-316D";
    fsType = "vfat";
  };

  fileSystems."/home/user/Games" = {
    device = "/dev/disk/by-uuid/9eee903d-0dff-4c49-83db-34c75b2974af";
    fsType = "btrfs";
    options = [ 
      "subvol=/@games"
      "compress-force=zstd:15"
    ];
  };

  fileSystems."/mnt/btrfs/nixos" = {
    device = "/dev/disk/by-uuid/43bf7935-ca90-4d0d-8d60-3c19ca8d5584";
    fsType = "btrfs";
    options = [
      "subvol=/"
      "compress-force=zstd:15"
    ];
  };

  fileSystems."/mnt/btrfs/bulk" = { 
    device = "/dev/disk/by-uuid/9eee903d-0dff-4c49-83db-34c75b2974af";
    fsType = "btrfs";
    options = [
      "subvol=/"
      "compress-force=zstd:15"
    ];
  };

  swapDevices = [ ];

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
