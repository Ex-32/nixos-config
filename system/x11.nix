{ config, pkgs, lib, nixpkgs, ... }:

{
  services.xserver = {
    enable = true;
    autorun = false;
    displayManager.startx.enable = true;
    libinput = {
      touchpad.naturalScrolling = true;
      mouse.naturalScrolling = true;
    };
  };
}
