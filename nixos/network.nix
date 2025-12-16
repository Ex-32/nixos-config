{
  config,
  pkgs,
  lib,
  nixpkgs,
  ...
}: {
  environment.systemPackages = with pkgs; [
    openconnect_openssl
  ];

  # NetworkManager: networking for noobs since 2004
  networking.networkmanager.enable = true;

  networking.firewall = let
    allowed = [
      4242 # nebula
      28785 # super tux kart
      57621 # spotify sync
      35666 # EmptyEpsilon
      1443 # anytype
    ];
  in {
    enable = true;
    allowedTCPPorts = allowed;
    allowedUDPPorts = allowed;
  };
}
