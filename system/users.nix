{ config, pkgs, lib, nixpkgs, ... }:

{
  users.users.jenna = {
    isNormalUser = true;
    description = "Jenna Fligor";
    extraGroups = [ "networkmanager" "wheel" "video" ];
    shell = pkgs.fish;
  };
}
