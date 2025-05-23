{
  config,
  pkgs,
  lib,
  nixpkgs,
  ...
}: {
  environment.systemPackages = with pkgs; [vial via];

  # udev rule to recognize vial devices and allow them to be configured
  services.udev.extraRules =
    # udev
    ''
      KERNEL=="hidraw*", SUBSYSTEM=="hidraw", ATTRS{serial}=="*vial:f64c2b3c*", MODE="0660", GROUP="users", TAG+="uaccess", TAG+="udev-acl"

      KERNEL=="hidraw*", SUBSYSTEM=="hidraw", MODE="0660", GROUP="users", TAG+="uaccess", TAG+="udev-acl"
    '';
}
