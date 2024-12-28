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

  hardware.graphics = {
    enable = true;
    enable32Bit = true;
  };
  services.xserver.videoDrivers = ["nvidia"];
  hardware.nvidia = {
    modesetting.enable = true;
    open = true;
  };
  hardware.nvidia-container-toolkit.enable = true;

  boot.initrd = {
    availableKernelModules = [
      "nvidia"
      "nvidia_drm"
      "nvidia_modeset"
      "nvidia_uvm"
    ];
    kernelModules = ["nvidia_drm"];
  };
}
