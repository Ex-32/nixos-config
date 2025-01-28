{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: let
  devs = {
    boot = "/dev/disk/by-uuid/22D7-AF2F";
    swap = "/dev/disk/by-uuid/526548fc-b325-4443-8c6e-f132ebf4e190";
  };
in {
  imports = [
    inputs.nixos-hardware.nixosModules.framework-13th-gen-intel
    ../nixos/zfs.nix
  ];

  networking = {
    hostName = "reason";
    hostId = "452fa516";
    useDHCP = lib.mkDefault true;
  };

  services.nebula.networks.Caldwell69 = {
    enable = true;
    lighthouses = ["192.168.69.1"];
    staticHostMap = {"192.168.69.1" = ["146.190.187.143:4242"];};
    firewall = {
      inbound = [
        {
          host = "any";
          port = "any";
          proto = "any";
        }
      ];
      outbound = [
        {
          host = "any";
          port = "any";
          proto = "any";
        }
      ];
    };
    cert = ../secrets/nebula/reason.crt;
    key = ../secrets/nebula/reason.key;
    ca = ../secrets/nebula/ca.crt;
  };

  # this enables firmware that's distributed as a redistributable binary but
  # not FOSS, not having this enabled can cause issues with some hardware,
  # especially wifi cards
  hardware = rec {
    enableRedistributableFirmware = true;
    # update microcode with break without enableRedistributableFirmware
    cpu.intel.updateMicrocode = enableRedistributableFirmware;
  };

  boot = {
    kernelParams = [
      "vsyscall=none"
      "consoleblank=60"
    ];
    kernelModules = ["kvm-intel"];
    extraModulePackages = [];

    initrd = {
      systemd.enable = true;
      availableKernelModules = ["xhci_pci" "thunderbolt" "nvme" "usb_storage" "sd_mod"];
      kernelModules = ["i915"];
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
          "z-00-efi-shell.conf" = ''
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
    dataset = subpath: {
      fsType = "zfs";
      device = "rpool/encrypt/${subpath}";
      neededForBoot = true;
    };
  in {
    "/boot" = {device = devs.boot;};

    "/nix" = dataset "volatile/nix";

    "/persist/safe/system" = dataset "safe/system";
    "/persist/safe/home" = dataset "safe/home";

    "/persist/volatile/cache" = dataset "volatile/cache";
    "/persist/volatile/games" = dataset "volatile/games";
  };

  swapDevices = [
    {device = devs.swap;}
  ];

  services.fprintd.enable = true;

  # fan/power optimization for laptop
  services.thermald.enable = true;
  services.tlp = {
    enable = true;
    settings = {
      MEM_SLEEP_ON_AC = "deep";
      MEM_SLEEP_ON_BAT = "deep";

      CPU_SCALING_GOVERNOR_ON_AC = "performance";
      CPU_SCALING_GOVERNOR_ON_BAT = "powersave";

      CPU_ENERGY_PERF_POLICY_ON_BAT = "power";
      CPU_ENERGY_PERF_POLICY_ON_AC = "performance";

      CPU_MIN_PERF_ON_AC = 0;
      CPU_MAX_PERF_ON_AC = 100;
      CPU_MIN_PERF_ON_BAT = 0;
      CPU_MAX_PERF_ON_BAT = 70;

      CPU_BOOST_ON_AC = 1;
      CPU_BOOST_ON_BAT = 0;
    };
  };

  # HACK: this stops errors about too many open files when emacs,
  # obsidian, or steam (probably others too) are open, but causes a
  # performance hit; remove when fixed upstream
  environment.variables.MESA_SHADER_CACHE_DISABLE = "true";

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
}
