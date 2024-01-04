{ config, pkgs, lib, nixpkgs, ... }:

{
  # switch from using the default LTS kernel to the latest stable kernel,
  # there's no really good reason to do this, other than my compulsive urge to
  # run the latest release of everything (this flake does track nixos-unstable
  # after all :P)
  boot.kernelPackages = pkgs.linuxPackages_latest;
}
