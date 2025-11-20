{
  config,
  pkgs,
  lib,
  nixpkgs,
  ...
}: {
  virtualisation.libvirtd = {
    enable = true;
    qemu.vhostUserPackages = [pkgs.virtiofsd];
  };
  programs.virt-manager.enable = true;
}
