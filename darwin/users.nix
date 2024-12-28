{
  config,
  pkgs,
  lib,
  nixpkgs,
  ...
}: {
  users.users.jenna = {
    uid = 501;
    home = /Users/jenna;
    description = "Jenna Fligor";
    shell = pkgs.nushell;
  };
}
