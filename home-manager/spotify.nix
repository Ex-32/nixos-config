{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  allowedUnfree = ["spotify"];
  home.packages= [pkgs.spotify];
}
