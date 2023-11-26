{ config, pkgs, lib, nixpkgs, ... }:

{
  environment.systemPackages = with pkgs; [ ecryptfs ];
  security.pam.enableEcryptfs = true;
  boot.kernelModules = [ "ecryptfs" ];
}
