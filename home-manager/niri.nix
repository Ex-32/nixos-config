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
    pkgs.writeScript "hyprland-wallpaper"
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
  xdg.configFile."niri/config.kdl".source = let
    wl-copy = lib.getExe' pkgs.wl-clipboard "wl-copy";
    grim = lib.getExe pkgs.grim;
    systemctl = lib.getExe' pkgs.systemd "systemctl";
  in
    pkgs.replaceVars ../config/niri/config.kdl rec {
      # @variables@ to substitute
      ## packages
      # brightnessctl = lib.getExe pkgs.brightnessctl;
      kitty = lib.getExe pkgs.kitty;
      # loginctl = lib.getExe' pkgs.systemd "loginctl";
      # playerctl = lib.getExe pkgs.playerctl;
      wpctl = lib.getExe' pkgs.wireplumber "wpctl";
      fuzzel = lib.getExe pkgs.fuzzel;
      # change_wallpaper = wallpaper-script;
      # screenshot_full =
      #   pkgs.writeScript "screenshot-full"
      #   #bash
      #   ''
      #     #!/bin/sh
      #     ${grim} - | ${wl-copy}
      #   '';
      # screenshot_select =
      #   pkgs.writeScript "screenshot-select"
      #   #bash
      #   ''
      #     #!/bin/sh
      #     ${lib.getExe pkgs.slurp} | ${grim} -g - - | ${wl-copy}
      #   '';
      toggle_activate_linux =
        pkgs.writeScript "toggle-activate-linux"
        # bash
        ''
          #!/bin/sh
          test "$(${systemctl} --user is-active activate-linux.service)" = "active" \
            && ${systemctl} --user stop activate-linux.service \
            || ${systemctl} --user start activate-linux.service
        '';
      vol_down =
        pkgs.writeScript "vol-down"
        # python
        ''
          #!${lib.getExe pkgs.python3}

          import subprocess

          output = subprocess.run(
            ["${wpctl}", "get-volume", "@DEFAULT_AUDIO_SINK@"],
            capture_output=True,
          ).stdout

          # between '0' and '9' ascii
          vol = int(bytes([x for x in output if x >= 48 and x <= 57]))
          new_vol = (((vol - 1) // 5) * 5) / 100

          subprocess.run(
            ["${wpctl}", "set-volume", "-l", "1.5", "@DEFAULT_AUDIO_SINK@", str(new_vol)]
          )
        '';
      vol_up =
        pkgs.writeScript "vol-up"
        # python
        ''
          #!${lib.getExe pkgs.python3}

          import subprocess

          output = subprocess.run(
            ["${wpctl}", "get-volume", "@DEFAULT_AUDIO_SINK@"],
            capture_output=True,
          ).stdout

          # between '0' and '9' ascii
          vol = int(bytes([x for x in output if x >= 48 and x <= 57]))
          new_vol = (((vol + 5) // 5) * 5) / 100

          subprocess.run(
            ["${wpctl}", "set-volume", "-l", "1.5", "@DEFAULT_AUDIO_SINK@", str(new_vol)]
          )
        '';

      # this causes these values (those used by wpctl) to be excluded from the
      # validation check that makes sure there are no unsubstituted values.
      DEFAULT_AUDIO_SINK = null;
      DEFAULT_AUDIO_SOURCE = null;
    };
  home.packages = [pkgs.niri];

  systemd.user = {
    services = {
      activate-linux = {
        Unit = {
          Description = "Activate Linux";
          Wants = ["graphical-session.target"];
          After = ["graphical-session.target"];
        };

        Service = {
          Type = "simple";
          ExecStart = "${pkgs.activate-linux}/bin/activate-linux";
        };

        Install = {
          WantedBy = ["graphical-session.target"];
        };
      };
      swww-daemon = {
        Unit = {
          Description = "swww wallpaper daemon";
          Wants = ["graphical-session.target"];
          After = ["graphical-session.target"];
        };

        Service = {
          Type = "simple";
          ExecStart = "${pkgs.swww}/bin/swww-daemon";
          Restart = "on-failure";
          RestartSec = 1;
          TimeoutSec = "30s";
        };

        Install = {
          WantedBy = ["graphical-session.target"];
        };
      };
      swww = {
        Unit = {
          Description = "cycle swww wallpaper";
          Wants = ["graphical-session.target"];
          After = [
            "graphical-session.target"
            "swww-daemon.service"
          ];
        };

        Service = {
          Type = "oneshot";
          ExecStart = "${wallpaper-script}";
        };

        Install = {
          WantedBy = ["graphical-session.target"];
        };
      };
    };
    timers = {
      swww = {
        Unit = {
          Description = "cycle swww wallpaper";
          Wants = ["graphical-session.target"];
          After = [
            "graphical-session.target"
            "swww-daemon.service"
          ];
        };

        Timer = {
          OnUnitActiveSec = "5min";
          RandomizedDelaySec = "30s";
          AccuracySec = "30s";
        };

        Install = {
          WantedBy = ["graphical-session.target"];
        };
      };
    };
  };
}
