{
  inputs,
  config,
  pkgs,
  lib,
  nixpkgs,
  ...
}: {
  imports = [
    inputs.sops-nix.nixosModules.sops
    ./ssh.nix # we use ssh host keys, so we need ssh to be enabled
  ];

  sops = {
    defaultSopsFile = ../secrets.yaml;
    age.sshKeyPaths = [
      "/persist/safe/system/etc/ssh/ssh_host_ed25519_key"
    ];
  };
}
