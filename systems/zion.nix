{
  config,
  lib,
  pkgs,
  ...
}: let
  devs = {
    boot = "/dev/disk/by-uuid/F045-9B7A";
    swap = "/dev/disk/by-uuid/77e77df0-ec36-4bf8-9bb0-a370ed6bd557";
  };
in {
  imports = [
    ../nixos/nvidia.nix
    ../nixos/zfs.nix
  ];

  networking = {
    hostName = "zion";
    hostId = "6f02efe2";
    useDHCP = lib.mkDefault true;
  };

  # this enables firmware that's distributed as a redistributable binary but
  # not FOSS, not having this enabled can cause issues with some hardware,
  # especially wifi cards
  hardware = rec {
    enableRedistributableFirmware = true;
    cpu.amd.updateMicrocode = enableRedistributableFirmware;
  };

  boot = {
    kernelParams = [
      "vsyscall=none"
      "consoleblank=60"
    ];
    kernelModules = ["kvm-amd"];
    extraModulePackages = [];

    initrd = {
      systemd.enable = true;
      availableKernelModules = ["nvme" "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod"];
      kernelModules = [];
    };

    loader = {
      efi = {
        canTouchEfiVariables = true;
        efiSysMountPoint = "/boot";
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
  };

  fileSystems = let
    boot-dataset = subpath: {
      fsType = "zfs";
      device = "rpool/encrypt/${subpath}";
      neededForBoot = true;
    };
    dataset = subpath: {
      fsType = "zfs";
      device = "rpool/encrypt/${subpath}";
      options = ["nofail"];
    };
  in {
    "/boot" = {device = devs.boot;};

    "/nix" = boot-dataset "volatile/nix" [];

    "/persist/safe/system" = boot-dataset "safe/system";
    "/persist/safe/home" = dataset "safe/home";

    "/persist/volatile/cache" = dataset "volatile/cache";
    "/persist/volatile/games" = dataset "volatile/games";
  };

  swapDevices = [
    {device = devs.swap;}
  ];

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
}
