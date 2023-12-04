{ config, pkgs, lib, nixpkgs, ... }:

{
  users.users.jenna = {
    isNormalUser = true;
    description = "Jenna Fligor";
    extraGroups = [ "networkmanager" "wheel" "video" "lp" "dialout" ];
    shell = pkgs.fish;
    initialHashedPassword = import ../secrets/passwd/jenna;
  };
}
