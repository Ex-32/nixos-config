{
  osConfig,
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  imports = [
    inputs.sops-nix.homeManagerModules.default
  ];

  sops = {
    defaultSopsFile = ../secrets.yaml;
    # FIXME: don't use admin key for user secrets
    age.keyFile = "${config.xdg.configHome}/sops/age/keys.txt";
  };
}
