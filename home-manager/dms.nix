{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: let
  dash-shebang = "#!" + lib.getExe pkgs.dash;
in {
  imports = [
    ./gtk.nix
    ./kitty.nix
    ./qt.nix
    ./fuzzel.nix
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
}
