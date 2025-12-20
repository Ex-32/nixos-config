{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  services.remmina = {
    enable = true;
    addRdpMimeTypeAssoc = true;
  };
}
