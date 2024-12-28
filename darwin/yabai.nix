{
  config,
  pkgs,
  lib,
  nixpkgs,
  ...
}: {
  services.yabai = {
    enable = true;
  };
}
