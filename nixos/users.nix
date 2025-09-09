{
  inputs,
  config,
  pkgs,
  lib,
  ...
}: {
  # because of impermanence, /etc/passwd and /etc/group are created from
  # scratch on each boot and then cease to exist on poweroff, so any changes
  # made wouldn't persist across boots, so it's best to just disallow it to
  # begin with
  users.mutableUsers = false;

  users.users.jenna = {
    isNormalUser = true;
    uid = 1000;
    description = "Jenna Fligor";
    extraGroups =
      [
        "networkmanager" # networking configuration privileges
        "wheel" # general admin (sudo) privileges
        "video" # raw video device access
        "lp" # printing privileges
        "dialout" # raw serial device access
        "nix" # nix access
        "kvm"
      ]
      ++ (lib.lists.optional config.services.jellyfin.enable "jellyfin")
      ++ (lib.lists.optional config.virtualisation.libvirtd.enable "libvirtd");
    shell = config.programs.xonsh.package;

    # without this any form of rootless containerization will fail
    autoSubUidGidRange = true;

    # impermanence means the password must be pre-specified, here we load it
    # from sops so the plaintext hash is never copied to the nix store.
    hashedPasswordFile = config.sops.secrets."login/jenna".path;

    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIG9fCOzAH3+OxW0bCwZy84Wh36lKVqVChsg0rAeTJmGZ"
    ];
  };
  sops.secrets."login/jenna".neededForUsers = true;
}
