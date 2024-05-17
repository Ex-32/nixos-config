{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ../lib/nvidia.nix
  ];

  hardware.enableRedistributableFirmware = true;

  boot.kernelParams = [
    "vsyscall=none"
    "consoleblank=60"
  ];

  boot.initrd.availableKernelModules = ["nvme" "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod"];
  boot.initrd.kernelModules = [];
  boot.kernelModules = ["kvm-amd"];
  boot.extraModulePackages = [];
  boot.swraid.enable = false;

  # this needs to be set even though / is a tmpfs because /tmp can't be noexec
  boot.tmp.useTmpfs = true;

  fileSystems."/nix" = {
    device = "/dev/disk/by-uuid/d043f002-e755-4a49-9316-58580bf9ec0a";
    fsType = "btrfs";
    options = [
      "compress=zstd"
      "subvol=@nix"
      "noatime"
      "nodev"
    ];
  };

  fileSystems."/persist" = {
    device = "/dev/disk/by-uuid/d043f002-e755-4a49-9316-58580bf9ec0a";
    fsType = "btrfs";
    options = [
      "subvol=/@nix-persist"
      "compress=zstd"
      "noatime"
      "nosuid"
      "nodev"
    ];
    neededForBoot = true;
  };

  fileSystems."/home" = {
    device = "/dev/disk/by-uuid/d043f002-e755-4a49-9316-58580bf9ec0a";
    fsType = "btrfs";
    options = [
      "compress=zstd"
      "subvol=@nix-home"
      "noatime"
      "nosuid"
      "nodev"
    ];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/d043f002-e755-4a49-9316-58580bf9ec0a";
    fsType = "btrfs";
    options = [
      "compress=zstd"
      "subvol=@nix-boot"
      "noatime"
      "nosuid"
      "nodev"
      "noexec"
    ];
  };

  fileSystems."/mnt/fsroot" = {
    device = "/dev/disk/by-uuid/d043f002-e755-4a49-9316-58580bf9ec0a";
    fsType = "btrfs";
    options = [
      "compress=zstd"
      "subvol=/"
      "noatime"
      "nosuid"
      "nodev"
      "noexec"
    ];
  };

  fileSystems."/boot/efi" = {
    device = "/dev/disk/by-uuid/6347-5502";
    fsType = "vfat";
    options = [
      "noatime"
      "nosuid"
      "nodev"
      "noexec"
    ];
  };

  swapDevices = [
    {device = "/dev/disk/by-uuid/f18767e1-f510-42e1-b2fb-8ea4ffd21329";}
  ];

  # non-critical disk mounts

  fileSystems."/mnt/bulk" = {
    device = "/dev/disk/by-uuid/81d0684c-1711-4b89-9e93-02116205a4ed";
    fsType = "btrfs";
    options = [
      "compress=zstd"
      "subvol=/"
      "noatime"
      "nosuid"
      "nodev"
      "noexec"
      "nofail"
    ];
  };

  fileSystems."/home/jenna/documents/games" = {
    device = "/dev/disk/by-uuid/81d0684c-1711-4b89-9e93-02116205a4ed";
    fsType = "btrfs";
    options = [
      "compress=zstd"
      "subvol=/@jenna-games"
      "noatime"
      "nosuid"
      "nofail"
    ];
  };

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
