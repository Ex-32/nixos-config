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
    extraGroups = [
      "networkmanager" # networking privileges
      "wheel"          # general admin (sudo) privileges
      "video"          # raw video device access
      "lp"             # printing privileges
      "dialout"        # raw serial device access
    ];
    shell = pkgs.nushell;

    # without this any form of rootless containerization will fail
    autoSubUidGidRange = true;

    # it must be initalHashedPassword, and not hashedPassword, because
    # impermanence means /etc/passwd exists on a ramdisk
    initialHashedPassword = import ../secrets/passwd/jenna;
  };
}
