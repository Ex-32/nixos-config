{
  config,
  lib,
  pkgs,
  ...
}: let
  devs = {
    boot = "/dev/disk/by-uuid/358E-B8BA";
    swap = "/dev/disk/by-uuid/5112d5ca-22a5-4f68-a60f-042f2141bffa";
  };
in {
  imports = [
    ../lib/nvidia.nix
    ../lib/zfs.nix
  ];

  # this enables firmware that's distributed as a redistributable binary but
  # not FOSS, not having this enabled can cause issues with some hardware,
  # especially wifi cards
  hardware.enableRedistributableFirmware = true;

  boot.kernelParams = [
    "vsyscall=none"
    "consoleblank=60"
  ];

  boot.initrd.availableKernelModules = ["nvme" "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod"];
  boot.initrd.kernelModules = [];
  boot.kernelModules = ["kvm-amd"];
  boot.extraModulePackages = [];

  networking.hostId = "6f02efe2";

  boot.loader = {
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

  services.ddclient = {
    enable = true;
    interval = "1h";
    use = "web, web=svc.joker.com/nic/checkip";
    server = "svc.joker.com/nic/update?";
    protocol = "dyndns2";
    username = import ../secrets/ddclient/nixos-pc/login;
    passwordFile = "/etc/nixos/secrets/ddclient/nixos-pc/password";
    domains = import ../secrets/ddclient/nixos-pc/domains;
    ssl = true;
  };

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
