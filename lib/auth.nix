{
  config,
  pkgs,
  lib,
  nixpkgs,
  ...
}: {
  # this disables the use of fprint (fingerprint auth) on tty login and
  # swaylock, the goal here is to require the typed password to unlock the
  # machine, and use the fingerprint only for privilege escalation requests
  # like polkit and sudo.
  security.pam.services.login.fprintAuth = false;
  security.pam.services.swaylock.fprintAuth = false;
  security.pam.services.hyprlock.fprintAuth = false;

  # this installs and enables a systemd user service for the mate polkit agent
  # (the mate agent was chosen because it works well with fprint, properly
  # follows the system themes, and doesn't have weird graphical glitches)
  systemd.user.services.polkit-agent = {
    description = "user polkit agent";
    wantedBy = ["graphical-session.target"];
    wants = ["graphical-session.target"];
    after = ["graphical-session.target"];
    serviceConfig = {
      Type = "simple";
      ExecStart = "${pkgs.mate.mate-polkit}/libexec/polkit-mate-authentication-agent-1";
      Restart = "on-failure";
      RestartSec = 1;
      RestartStopSec = 10;
    };
  };
}
