{
  config,
  pkgs,
  lib,
  nixpkgs,
  ...
}: {
  boot = {
    zfs.package = pkgs.zfs_unstable;
    kernelPackages = config.boot.zfs.package.latestCompatibleLinuxPackages;
    initrd.supportedFilesystems = ["zfs"];
    supportedFilesystems = ["zfs"];
  };
  services.zfs = {
    autoSnapshot = {
      enable = true;
      flags = "-k -p --utc";
      frequent =  8;
      hourly = 48;
      daily = 14;
      weekly = 8;
      monthly =  24;
    };
  };
}
