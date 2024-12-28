{
  config,
  pkgs,
  lib,
  nixpkgs,
  inputs,
  ...
}: {
  # this adds the nix-alien tool which uses a virtual FHS environment to allow
  # foreign dynamically linked programs to run. alternatively it can just
  # introspect the dynamic libraries needed by a program and set the
  # environment variables for nix-ld
  environment.systemPackages = [
    inputs.nix-alien.packages."${pkgs.system}".nix-alien
  ];

  # this adds a shim at /lib64/ld-linux-x86-64.so.2 that reads specific
  # environment variables and preloads those dynamic libraries from the nix
  # store, in conjunction with nix-alien this can run almost any binary, even
  # if not compiled in a nix environment
  programs.nix-ld.enable = true;
}
