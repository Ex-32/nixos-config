{
  config,
  inputs,
  lib,
  pkgs,
  modulesPath,
  ...
}: let
  devs = {
    boot = "/dev/disk/by-uuid/22D7-AF2F";
    swap = "/dev/disk/by-uuid/526548fc-b325-4443-8c6e-f132ebf4e190";
  };
in {
  imports = [
    inputs.nixos-hardware.nixosModules.framework-13th-gen-intel
  ];

  # this enables firmware that's distributed as a redistributable binary but
  # not FOSS, not having this enabled can cause issues with some hardware,
  # especially wifi cards
  hardware.enableRedistributableFirmware = true;

  boot.kernelParams = [
    "vsyscall=none"
    "consoleblank=60"
  ];

  boot.initrd.availableKernelModules = ["xhci_pci" "thunderbolt" "nvme" "usb_storage" "sd_mod"];
  boot.initrd.kernelModules = [];
  boot.kernelModules = ["kvm-intel"];
  boot.extraModulePackages = [];

  networking.hostId = "452fa516";
  boot.zfs.package = pkgs.zfs_unstable;

  boot.loader = {
    systemd-boot.enable = true;
    efi = {
      canTouchEfiVariables = true;
      efiSysMountPoint = "/boot";
    };

    systemd-boot = {
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
  services.logind.extraConfig = ''
    # short power-button press suspends the laptop rather than powering it off
    HandlePowerKey=suspend
  '';

  # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
  # (the default) this is the recommended approach. When using systemd-networkd it's
  # still possible to use this option, but it's recommended to use it in conjunction
  # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
  networking.useDHCP = lib.mkDefault true;
  # networking.interfaces.wlp170s0.useDHCP = lib.mkDefault true;

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
