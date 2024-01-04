{ config, pkgs, lib, nixpkgs, ... }:

{
  # because of impermanence, /etc/passwd and /etc/group are created from
  # scratch on each boot and then cease to exist on poweroff, so any changes
  # made wouldn't persist across boots, so it's best to just disallow it to
  # begin with
  users.mutableUsers = false;

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
