{
  config,
  osConfig,
  pkgs,
  lib,
  inputs,
  ...
}: let
  wallpaper-script = let
    core = let
      cu = pkgs.coreutils;
    in {
      od = lib.getExe' cu "od";
      ls = lib.getExe' cu "ls";
      printf = lib.getExe' cu "printf";
      shuf = lib.getExe' cu "shuf";
    };
    home = config.home.homeDirectory;
    hostname = osConfig.networking.hostName;
    rand-file = "/dev/urandom";
  in
    pkgs.writeScript "niri-wallpaper"
    # bash
    ''
      #!/bin/sh
      set -e
      # FIXME: don't hardcode wallpaper path
      WALLPAPER_PATH="${home}/documents/pictures/wallpapers/${hostname}"
      if [ ! -d "$WALLPAPER_PATH" ] ; then
        echo "wallpaper directory '$WALLPAPER_PATH' doesn't exist"
        exit 1
      fi
      rand_pos() {
        ${core.printf} '0.%02d' $(( $(${core.od} -An -N2 -d ${rand-file}) % 99 + 1 ))
      }
      RAND_PAPER="$WALLPAPER_PATH/$(${core.ls} -1 "$WALLPAPER_PATH" |\
        ${core.shuf} --random-source=${rand-file} -n 1)"
      ${lib.getExe pkgs.swww} img \
        --transition-duration 2 \
        --transition-type grow \
        --transition-pos "$(rand_pos),$(rand_pos)" \
        "$RAND_PAPER"
    '';
in {
  imports = [
    ./gtk.nix
    ./kitty.nix
    ./mako.nix
    ./qt.nix
    ./waybar.nix
    ./fuzzel.nix
  ];

  xdg.configFile."niri/config.kdl".source = let
    systemctl = lib.getExe' pkgs.systemd "systemctl";
  in
    pkgs.replaceVars ../config/niri/config.kdl {
      # @variables@ to substitute

      ## packages
      brightnessctl = lib.getExe pkgs.brightnessctl;
      playerctl = lib.getExe pkgs.playerctl;
      wpctl = lib.getExe' pkgs.wireplumber "wpctl";

      ## scripts
      startup_hook = let
        variables = builtins.concatStringsSep " " [
          "DISPLAY"
          "NIRI_SOCKET"
          "WAYLAND_DISPLAY"
          "XDG_CURRENT_DESKTOP"
          "XDG_SESSION_TYPE"
        ];
        systemctl = lib.getExe' pkgs.systemd "systemctl";
      in
        pkgs.writeScript "niri-startup-hook"
        # bash
        ''
          #!${lib.getExe pkgs.dash}

          i=0
          used_displays="$(for x in $(find /tmp/.X11-unix/ -not -type d); do basename "$x" ; done | sed -r 's/X([0-9]+)_?/\1/' | uniq)"

          while [ -z "$DISPLAY" ] ; do
            for curr in $used_displays ; do
              if [ $curr -eq $i ] ; then
                ((i++))
                continue 2
              fi
            done
            export DISPLAY=":$i"
          done
          echo "$DISPLAY" > "''${XDG_RUNTIME_DIR}/DISPLAY_''${WAYLAND_DISPLAY}"

          ${lib.getExe' pkgs.dbus "dbus-update-activation-environment"} --systemd ${variables}
          ${systemctl} --user import-environment ${variables}
          ${systemctl} --user stop niri-session.target
          ${systemctl} --user start niri-session.target
        '';
      launch_kitty =
        pkgs.writeScript "launch-kitty"
        # bash
        ''
          #!${lib.getExe pkgs.dash}
          eval "export DISPLAY=\"$(cat "''${XDG_RUNTIME_DIR}/DISPLAY_''${WAYLAND_DISPLAY}")\""
          exec ${lib.getExe pkgs.kitty} -1
        '';
      launch_fuzzel =
        pkgs.writeScript "launch-fuzzel"
        # bash
        ''
          #!${lib.getExe pkgs.dash}
          eval "export DISPLAY=\"$(cat "''${XDG_RUNTIME_DIR}/DISPLAY_''${WAYLAND_DISPLAY}")\""
          exec ${lib.getExe pkgs.fuzzel}
        '';
      change_wallpaper = wallpaper-script;
      toggle_activate_linux =
        pkgs.writeScript "toggle-activate-linux"
        # bash
        ''
          #!/bin/sh
          test "$(${systemctl} --user is-active activate-linux.service)" = "active" \
            && ${systemctl} --user stop activate-linux.service \
            || ${systemctl} --user start activate-linux.service
        '';

      # this causes these values (those used by wpctl) to be excluded from the
      # validation check that makes sure there are no unsubstituted values.
      DEFAULT_AUDIO_SINK = null;
      DEFAULT_AUDIO_SOURCE = null;
    };

  home.packages = with pkgs; [
    niri
    wl-clipboard
    wl-mirror
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

  programs.swaylock = {
    enable = true;
    settings = let
      transparent = "00000000";
      black = "000000";
      default = "ff006e";
      clear = "ff9ed9";
      caps-lock = "d6e800";
      ver = "7000fc";
      wrong = "ff1212";
      bs = "9c000e";
    in {
      bs-hl-color = bs;
      caps-lock-bs-hl-color = bs;
      caps-lock-key-hl-color = caps-lock;
      color = black;
      indicator-radius = 250;
      indicator-thickness = 20;
      inside-caps-lock-color = transparent;
      inside-clear-color = transparent;
      inside-color = transparent;
      inside-ver-color = transparent;
      inside-wrong-color = transparent;
      key-hl-color = black;
      line-uses-inside = true;
      ring-caps-lock-color = caps-lock;
      ring-clear-color = clear;
      ring-color = default;
      ring-ver-color = ver;
      ring-wrong-color = wrong;
      separator-color = transparent;
      text-caps-lock-color = caps-lock;
      text-clear-color = clear;
      text-color = default;
      text-ver-color = ver;
      text-wrong-color = wrong;
    };
  };

  services.swayidle = let
    swaylock = lib.getExe pkgs.swaylock;
  in {
    enable = true;
    events = [
      {
        event = "before-sleep";
        command = swaylock;
      }
    ];
    timeouts = [
      {
        timeout = 180;
        command = swaylock;
      }
    ];
  };

  systemd.user = let
    niri-target = "niri-session.target";
  in {
    targets.niri-session = {
      Unit = {
        Description = "niri compositor session";
        Documentation = ["man:systemd.special(7)"];
        BindsTo = ["graphical-session.target"];
        Wants = [
          "graphical-session-pre.target"
          "xdg-desktop-autostart.target"
        ];
        After = ["graphical-session-pre.target"];
        Before = ["xdg-desktop-autostart.target"];
      };
    };
    services = {
      activate-linux = {
        Unit = {
          Description = "Activate Linux";
          Wants = [niri-target];
          After = [niri-target];
        };

        Service = {
          Type = "simple";
          ExecStart = lib.getExe pkgs.activate-linux + " -s 0.8";
        };

        Install.WantedBy = [niri-target];
      };
      xwayland-satellite = {
        Unit = {
          Description = "xwayland satellite daemon";
          Wants = [niri-target];
          After = [niri-target];
        };

        Service = {
          Type = "simple";
          ExecStart = lib.getExe pkgs.xwayland-satellite + " $DISPLAY";
          Restart = "on-failure";
          RestartSec = 1;
          TimeoutSec = "30s";
        };

        Install.WantedBy = [niri-target];
      };
      swww-daemon = {
        Unit = {
          Description = "swww wallpaper daemon";
          Wants = [niri-target];
          After = [niri-target];
        };

        Service = {
          Type = "simple";
          ExecStart = lib.getExe' pkgs.swww "swww-daemon";
          Restart = "on-failure";
          RestartSec = 1;
          TimeoutSec = "30s";
        };

        Install.WantedBy = [niri-target];
      };
      swww = {
        Unit = {
          Description = "cycle swww wallpaper";
          Wants = [niri-target];
          After = [niri-target "swww-daemon.service"];
        };

        Service = {
          Type = "oneshot";
          ExecStart = wallpaper-script;
        };

        Install.WantedBy = [niri-target];
      };
    };
    timers = {
      swww = {
        Unit = {
          Description = "cycle swww wallpaper";
          Wants = [niri-target];
          After = [niri-target "swww-daemon.service"];
        };

        Timer = {
          OnUnitActiveSec = "5min";
          RandomizedDelaySec = "30s";
          AccuracySec = "30s";
        };

        Install.WantedBy = [niri-target];
      };
    };
  };
}
