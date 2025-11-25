{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: let
  devs = {
    esp = "/dev/disk/by-uuid/";
    boot = "/dev/disk/by-uuid/";
    swap = "/dev/disk/by-uuid/";
  };
in {
  imports = [
    ../nixos/zfs.nix
  ];

  networking = {
    hostName = "xdeck";
    hostId = "452fa516";
    useDHCP = lib.mkDefault true;
  };

  # this enables firmware that's distributed as a redistributable binary but
  # not FOSS, not having this enabled can cause issues with some hardware,
  # especially wifi cards
  hardware = rec {
    enableRedistributableFirmware = true;
    # update microcode with break without enableRedistributableFirmware
    # cpu.intel.updateMicrocode = enableRedistributableFirmware;
  };

  boot = {
    kernelParams = [
      "vsyscall=none"
      "consoleblank=60"
    ];
    # kernelModules = ["kvm-intel"];
    extraModulePackages = [];

    initrd = {
      systemd.enable = true;
      availableKernelModules = ["xhci_pci" "thunderbolt" "nvme" "usb_storage" "sd_mod"];
      kernelModules = ["dm-snapshot" "cryptd"];
      luks.devices."cryptroot" = {
        device = "";
        preLVM = true;
      };
    };

    loader = {
      efi = {
        canTouchEfiVariables = true;
        efiSysMountPoint = "/boot/efi/";
      };

      systemd-boot = {
        enable = true;
        memtest86.enable = true;
        extraFiles = {
          "efi/shell/shell.efi" = "${pkgs.edk2-uefi-shell}/shell.efi";
        };
        extraEntries = {
          "z-01-efi-shell.conf" = ''
            title EFI Shell
            efi /efi/shell/shell.efi
          '';
        };
      };
    };

    supportedFilesystems = [
      "exfat"
      "ntfs"
      "vfat"
      "zfs"
    ];
  };

  fileSystems = let
    boot-dataset = subpath: {
      fsType = "zfs";
      device = "rpool/${subpath}";
      neededForBoot = true;
    };
    dataset = subpath: {
      fsType = "zfs";
      device = "rpool/${subpath}";
      options = ["nofail"];
    };
  in {
    "/boot" = {device = devs.boot;};
    "/boot/efi" = {device = devs.esp;};

    "/nix" = boot-dataset "volatile/nix";

    "/persist/safe/system" = boot-dataset "safe/system";
    "/persist/safe/home" = dataset "safe/home";

    "/persist/volatile/cache" = dataset "volatile/cache";
    "/persist/volatile/games" = dataset "volatile/games";
  };

  swapDevices = [
    {device = devs.swap;}
  ];

  services.lvm.enable = true;

  # fan/power optimization for laptop
  # services.thermald.enable = true;
  # services.tlp = {
  #   enable = true;
  #   settings = {
  #     MEM_SLEEP_ON_AC = "deep";
  #     MEM_SLEEP_ON_BAT = "deep";
  #
  #     CPU_SCALING_GOVERNOR_ON_AC = "performance";
  #     CPU_SCALING_GOVERNOR_ON_BAT = "powersave";
  #
  #     CPU_ENERGY_PERF_POLICY_ON_BAT = "power";
  #     CPU_ENERGY_PERF_POLICY_ON_AC = "performance";
  #
  #     CPU_MIN_PERF_ON_AC = 0;
  #     CPU_MAX_PERF_ON_AC = 100;
  #     CPU_MIN_PERF_ON_BAT = 0;
  #     CPU_MAX_PERF_ON_BAT = 70;
  #
  #     CPU_BOOST_ON_AC = 1;
  #     CPU_BOOST_ON_BAT = 0;
  #   };
  # };

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
}
