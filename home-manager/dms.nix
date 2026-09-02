{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: let
  dash-shebang = "#!" + lib.getExe pkgs.dash;
  dms = pkgs.dms-shell;
in {
  imports = [
    ./gtk.nix
    ./kitty.nix
    ./qt.nix
    ./fuzzel.nix
    ./systray.nix
  ];

  wayland.windowManager.niri = {
    enable = true;
    extraConfig = builtins.readFile (pkgs.replaceVars ../config/niri/config.kdl rec {
      # @variables@ to substitute

      ## packages
      brightnessctl = lib.getExe pkgs.brightnessctl;
      fuzzel = lib.getExe pkgs.fuzzel;
      kitty = lib.getExe pkgs.kitty;
      playerctl = lib.getExe pkgs.playerctl;
      systemctl = lib.getExe' pkgs.systemd "systemctl";
      wpctl = lib.getExe' pkgs.wireplumber "wpctl";

      # ## scripts
      # startup_hook = let
      #   variables = builtins.concatStringsSep " " [
      #     "NIRI_SOCKET"
      #     "WAYLAND_DISPLAY"
      #     "XDG_CURRENT_DESKTOP"
      #     "XDG_SESSION_TYPE"
      #   ];
      # in
      #   pkgs.writeScript "niri-startup-hook"
      #   # bash
      #   ''
      #     ${dash-shebang}
      #     ${lib.getExe' pkgs.dbus "dbus-update-activation-environment"} --systemd ${variables}
      #     ${systemctl} --user import-environment ${variables}
      #     ${systemctl} --user stop niri-session.target
      #     ${systemctl} --user start niri-session.target
      #   '';
      # change_wallpaper = wallpaper-script;
      # toggle_activate_linux =
      #   pkgs.writeScript "toggle-activate-linux"
      #   # bash
      #   ''
      #     ${dash-shebang}
      #     test "$(${systemctl} --user is-active activate-linux.service)" = "active" \
      #       && ${systemctl} --user stop activate-linux.service \
      #       || ${systemctl} --user start activate-linux.service
      #   '';

      # this causes these values (those used by wpctl) to be excluded from the
      # validation check that makes sure there are no unsubstituted values.
      DEFAULT_AUDIO_SINK = null;
      DEFAULT_AUDIO_SOURCE = null;
    });
  };

  home.packages = [
    dms
    pkgs.dgop
    pkgs.quickshell
  ];

  xdg.portal = {
    enable = true;
    extraPortals = with pkgs; [
      xdg-desktop-portal-gnome
      xdg-desktop-portal-gtk
    ];
    config.common = {
      default = ["gtk"];
      "org.freedesktop.impl.portal.ScreenCast" = ["gnome"];
      "org.freedesktop.impl.portal.Secret" = ["gnome-keyring"];
    };
  };

  systemd.user = let
    graphical-target = "graphical-session.target";
  in {
    services = {
      dms = {
        Unit = {
          Description = "DankMaterialShell Graphical Shell";
          Wants = [graphical-target];
          After = [graphical-target];
        };

        Service = {
          Type = "simple";
          ExecStart = lib.getExe dms + " run";
        };

        Install.WantedBy = [graphical-target];
      };
      activate-linux = {
        Unit = {
          Description = "Activate Linux";
          Wants = [graphical-target];
          After = [graphical-target];
        };

        Service = {
          Type = "simple";
          ExecStart = lib.getExe pkgs.activate-linux + " -s 0.8";
        };

        Install.WantedBy = [graphical-target];
      };
    };
  };
}
