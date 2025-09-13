{
  config,
  pkgs,
  lib,
  nixpkgs,
  ...
}: {
  allowedUnfree = [
    "via"
  ];

  environment.systemPackages = [pkgs.vial];

  # create a hidraw group for hidraw devices
  users.groups = {hidraw = {};};

  # make all hidraw devices 0660 for root:hidraw
  services.udev.extraRules =
    # udev
    ''
      KERNEL=="hidraw*", SUBSYSTEM=="hidraw", MODE="0660", GROUP="hidraw", TAG+="uaccess", TAG+="udev-acl"
    '';
}
