{
  config,
  pkgs,
  lib,
  nixpkgs,
  ...
}: {
  # this enables podman as a backend for distrobox, podman is used over docker
  # because it supports non-root containers; since the containers are are used
  # interactively and have broad filesystem access, running the container as
  # root is inadvisable for the same reason that logging in as root, or running
  # a graphical environment as root is inadvisable. While docker does have
  # support for rootless containers it seems to exhibit buggy behavior with
  # distrobox
  virtualisation.podman.enable = true;

  environment = {
    systemPackages = [pkgs.distrobox];
    etc."distrobox/distrobox.conf".text = ''
      container_additional_volumes="${builtins.concatStringsSep " " [
        "/nix/store:/nix/store:ro"
        "/etc/profiles/per-user:/etc/profiles/per-user:ro"
        "/etc/static/profiles/per-user:/etc/static/profiles/per-user:ro"
      ]}"
    '';
  };
}
