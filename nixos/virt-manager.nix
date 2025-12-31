{
  config,
  pkgs,
  lib,
  nixpkgs,
  ...
}: {
  virtualisation.libvirtd = {
    enable = true;
    qemu = {
      vhostUserPackages = [pkgs.virtiofsd];
      swtpm.enable = true;
    };
  };
  programs.virt-manager.enable = true;
}
