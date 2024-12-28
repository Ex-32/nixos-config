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
      28785 # super tux kart
      57621 # spotify sync
    ];
  in {
    enable = true;
    allowedTCPPorts = allowed;
    allowedUDPPorts = allowed;
  };
}
