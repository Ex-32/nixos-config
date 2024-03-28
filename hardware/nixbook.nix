{
  inputs,
  config,
  lib,
  pkgs,
  modulesPath,
  ...
}: {
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    inputs.nixos-hardware.nixosModules.framework-13th-gen-intel
    ./grub-patch.nix
  ];

  boot.blacklistedKernelModules = [
    "hid_sensor_hub"
  ];
  boot.kernelParams = [
    "mem_sleep_default=deep"
    "vsyscall=none"
    "consoleblank=60"
  ];

  boot.initrd.availableKernelModules = ["xhci_pci" "thunderbolt" "nvme" "usb_storage" "sd_mod"];
  boot.initrd.kernelModules = [];
  boot.kernelModules = ["kvm-intel"];
  boot.extraModulePackages = [];

  # fix warning do to stateVersion change
  boot.swraid.enable = false;

  boot.initrd = {
    luks.devices."cryptdisk" = {
      device = "/dev/disk/by-uuid/6ddff834-5606-48b9-a485-32dd6bdd6b79";
      keyFile = "/crypto_keyfile.bin";
    };
    secrets = {
      "/crypto_keyfile.bin" = "/persist/secrets/crypto_keyfile.bin";
    };
  };

  fileSystems = {
    "/home" = {
      device = "/dev/disk/by-uuid/fde120e0-e51e-4d41-8d7f-7edb4bf3b4ef";
      fsType = "btrfs";
      options = [
        "subvol=/@home"
        "compress=zstd"
        "noatime"
        "nosuid"
        "nodev"
      ];
    };
    "/nix" = {
      device = "/dev/disk/by-uuid/fde120e0-e51e-4d41-8d7f-7edb4bf3b4ef";
      fsType = "btrfs";
      options = [
        "subvol=/@nix"
        "compress=zstd"
        "noatime"
      ];
    };
    "/persist" = {
      device = "/dev/disk/by-uuid/fde120e0-e51e-4d41-8d7f-7edb4bf3b4ef";
      fsType = "btrfs";
      options = [
        "subvol=/@nix-persist"
        "compress=zstd"
        "noatime"
        "nosuid"
        "nodev"
        "noexec"
      ];
      neededForBoot = true;
    };
    "/mnt/fsroot" = {
      device = "/dev/disk/by-uuid/fde120e0-e51e-4d41-8d7f-7edb4bf3b4ef";
      fsType = "btrfs";
      options = [
        "subvol=/"
        "compress=zstd"
        "noatime"
        "nosuid"
        "nodev"
        "noexec"
      ];
    };
    "/boot" = {
      device = "/dev/disk/by-uuid/fde120e0-e51e-4d41-8d7f-7edb4bf3b4ef";
      fsType = "btrfs";
      options = [
        "subvol=/@boot"
        "compress=zstd"
        "noatime"
        "nosuid"
        "nodev"
        "noexec"
      ];
    };
    "/boot/efi" = {
      device = "/dev/disk/by-uuid/18A2-E6E3";
      fsType = "vfat";
      options = [
        "noatime"
        "nosuid"
        "nodev"
        "noexec"
      ];
    };
  };

  swapDevices = [];

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
      CPU_MAX_PERF_ON_BAT = 30;

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
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
