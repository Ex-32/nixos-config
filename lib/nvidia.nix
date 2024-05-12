{
  config,
  pkgs,
  lib,
  nixpkgs,
  ...
}: {
  allowedUnfree = [
    "nvidia-x11"
    "nvidia-settings"
  ];

  hardware.opengl = {
    enable = true;
    driSupport = true;
    driSupport32Bit = true;
  };
  services.xserver.videoDrivers = ["nvidia"];
  hardware.nvidia.modesetting.enable = true;
  hardware.nvidia-container-toolkit.enable = true;
}
