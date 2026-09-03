{
  config,
  pkgs,
  lib,
  nixpkgs,
  ...
}: {
  allowedUnfree = [
    "zerotierone"
  ];

  services.zerotierone = {
    enable = true;
    joinNetworks = [
      "166359304ed70de4"
    ];
  };
}
