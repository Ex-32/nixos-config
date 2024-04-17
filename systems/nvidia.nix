{
  config,
  pkgs,
  lib,
  nixpkgs,
  ...
}: {
  hardware.opengl = {
    enable = true;
    driSupport = true;
    driSupport32Bit = true;
  };
  services.xserver.videoDrivers = ["nvidia"];
  hardware.nvidia = {
    modesetting.enable = true;
  };

  virtualisation.containers.cdi.dynamic.nvidia.enable = true;
}
