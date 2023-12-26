{ config, pkgs, lib, nixpkgs, ... }:

{
  # switch from using the default LTS kernel to the latest stable kernel
  boot.kernelPackages = pkgs.linuxPackages_latest;
}
