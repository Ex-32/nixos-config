{ config, pkgs, lib, nixpkgs, ... }:

{
  users.users.jenna = {
    isNormalUser = true;
    uid = 1000;
    description = "Jenna Fligor";
    extraGroups = [ "networkmanager" "wheel" "video" "lp" "dialout" ];
    shell = pkgs.fish;

    # it must be initalHashedPassword, and not hashedPassword, because
    # impermanence means /etc/passwd exists on a ramdisk
    initialHashedPassword = import ../secrets/passwd/jenna;
  };
}
