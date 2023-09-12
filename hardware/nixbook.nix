# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ config, lib, pkgs, modulesPath, ... }:

{
  imports =
    [ (modulesPath + "/installer/scan/not-detected.nix")
    ];

  boot.blacklistedKernelModules = [
      "hid_sensor_hub    "
  ];
  boot.kernelParams = [
      "mem_sleep_default=deep"
      "vsyscall=none"
      "consoleblank=60"
  ];


  boot.initrd.availableKernelModules = [ "xhci_pci" "thunderbolt" "nvme" "usb_storage" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/fde120e0-e51e-4d41-8d7f-7edb4bf3b4ef";
      fsType = "btrfs";
      options = [
          "subvol=/@"
          "compress=zstd"
      ];
    };

  boot.initrd.luks.devices."cryptdisk" = {
    device = "/dev/disk/by-uuid/6ddff834-5606-48b9-a485-32dd6bdd6b79";
    keyFile = "/crypto_keyfile.bin";
  };

  boot.initrd.secrets = {
      "/crypto_keyfile.bin" = "/root/secrets/crypto_keyfile.bin";
  };

  fileSystems."/home" =
    { device = "/dev/disk/by-uuid/fde120e0-e51e-4d41-8d7f-7edb4bf3b4ef";
      fsType = "btrfs";
      options = [
          "subvol=/@home"
          "compress=zstd"
      ];
    };

  fileSystems."/var/log" =
    { device = "/dev/disk/by-uuid/fde120e0-e51e-4d41-8d7f-7edb4bf3b4ef";
      fsType = "btrfs";
      options = [
          "subvol=/@logs"
          "compress=zstd"
      ];
    };

  fileSystems."/nix" =
    { device = "/dev/disk/by-uuid/fde120e0-e51e-4d41-8d7f-7edb4bf3b4ef";
      fsType = "btrfs";
      options = [
          "subvol=/@nix"
          "compress=zstd"
      ];
    };

  fileSystems."/mnt/fsroot" =
    { device = "/dev/disk/by-uuid/fde120e0-e51e-4d41-8d7f-7edb4bf3b4ef";
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
