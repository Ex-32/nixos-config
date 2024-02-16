{ config, pkgs, lib, nixpkgs, ... }:

{
  # this automatically decrypts and mounts a home directory encrypted with
  # ecryptfs on login, which is my only use-case for ecryptfs (for general
  # encrypted directories gocryptfs is more stable and easier to use)
  security.pam.enableEcryptfs = true;
  environment.systemPackages = [ pkgs.ecryptfs ];
  boot.kernelModules = [ "ecryptfs" ];
}
