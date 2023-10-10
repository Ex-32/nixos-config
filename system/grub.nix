{ config, pkgs, lib, nixpkgs, ... }:

{
  boot.loader.efi = {
    canTouchEfiVariables = true;
    efiSysMountPoint = "/boot/efi";
  };
  boot.loader.grub = {
    enable = true;
    device = "nodev";
    efiSupport = true;
    enableCryptodisk = true;
    font = "${pkgs.spleen}/share/fonts/misc/spleen-16x32.otf";
    fontSize = 32;
  };
}
