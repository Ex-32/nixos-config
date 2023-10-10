{ config, pkgs, lib, nixpkgs, ... }:

{
  environment.systemPackages = with pkgs; [ mate.mate-polkit ];
  systemd.user.services.polkit-agent = {
    description = "user polkit agent";
    wantedBy = [ "graphical-session.target" ];
    wants = [ "graphical-session.target" ];
    after = [ "graphical-session.target" ];
    serviceConfig = {
        Type = "simple";
        ExecStart = "${pkgs.mate.mate-polkit}/libexec/polkit-mate-authentication-agent-1";
        Restart = "on-failure";
        RestartSec = 1;
        RestartStopSec = 10;
    };
  };
}
