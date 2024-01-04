{ config, pkgs, lib, nixpkgs, ... }:

{
  environment.systemPackages = with pkgs; [ vial ];

  # udev rule to recognize vial devices and allow them to be configured
  services.udev.extraRules = ''
    KERNEL=="hidraw*", SUBSYSTEM=="hidraw", ATTRS{serial}=="*vial:f64c2b3c*", MODE="0660", GROUP="users", TAG+="uaccess", TAG+="udev-acl"
  '';
}
