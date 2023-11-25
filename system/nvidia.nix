{ config, pkgs, lib, nixpkgs, ... }:

{
  hardware.nvidia.modesetting.enable = true;
  services.xserver.videoDrivers = [ "nvidia" ];
}
