{ config, pkgs, lib, inputs, ... }:

{
  services.picom = {
    enable = true;
    backend = "glx";
    vSync = true;
  };
}
