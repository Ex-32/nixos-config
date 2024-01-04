{ config, pkgs, lib, nixpkgs, ... }:

{
  services.openssh = {
    enable = true;
    startWhenNeeded = true;

    # given the number of automated port 22 spamming bots out there, i prefer
    # to keep my ssh on another port, i don't remember why i originally chose
    # this one, but it's my default go-to for ssh now
    ports = [ 3932 ];

    # i'm dumb not stupid
    settings.PermitRootLogin = "no";
  };
}
