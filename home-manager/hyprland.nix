{
  config,
  osConfig,
  pkgs,
  lib,
  inputs,
  ...
}: let
  wallpaper-script = let
    core = "${pkgs.coreutils}/bin";
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
        ${core}/printf '0.%02d' $(( $(${core}/od -An -N2 -d ${rand-file}) % 99 + 1 ))
      }
      RAND_PAPER="$WALLPAPER_PATH/$(${core}/ls -1 "$WALLPAPER_PATH" |\
        ${core}/shuf --random-source=${rand-file} -n 1)"
      ${pkgs.swww}/bin/swww img \
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
    ./systray.nix
    ./waybar-hyprland.nix
    ./fuzzel.nix
  ];

  home.packages = with pkgs; [
    wl-clipboard
    wl-mirror
    xdg-desktop-portal
    xdg-desktop-portal-gtk
    xdg-desktop-portal-hyprland
  ];

  wayland.windowManager.hyprland = {
    enable = true;
    systemd.enable = true;
    settings = {
      "$mod" = "SUPER";
      general = {
        gaps_in = 3;
        gaps_out = 6;
        border_size = 3;
        "col.active_border" = "rgb(d6e800) rgb(ff006e) 70deg";
        "col.inactive_border" = "0xff272725";
        layout = "master";
      };
      input = {
        kb_layout = "us";
        kb_options = "compose:ralt,ctrl:nocaps";
        natural_scroll = true;
        touchpad = {
          natural_scroll = true;
          tap-to-click = false;
          disable_while_typing = false;
          clickfinger_behavior = true;
        };
        sensitivity = 0.6;
      };
      decoration = {
        rounding = 0;
        blur = {
          enabled = true;
          size = 2;
        };
        shadow = {
          enabled = false;
        };
      };
      animations = {
        enabled = true;
        bezier = [
          "myBezier, 0.05, 0.9, 0.1, 1.05"
        ];
        animation = [
          "windows, 1, 4, myBezier"
          "windowsOut, 1, 4, default, popin 80%"
          "border, 1, 4, default"
          "borderangle, 1, 2, default"
          "fade, 1, 4, default"
          "workspaces, 1, 4, default"
          "specialWorkspace, 1, 2, default, slidevert"
        ];
      };
      gestures = {
        workspace_swipe = true;
        workspace_swipe_forever = true;
        workspace_swipe_direction_lock = false;
        # workspace_swipe_numbered = true;
        workspace_swipe_distance = 250;
        workspace_swipe_min_speed_to_force = 10;
      };
      misc = {
        disable_hyprland_logo = true;
        disable_splash_rendering = true;
      };
      dwindle = {
        special_scale_factor = 0.9;
      };
      master = {
        special_scale_factor = 0.9;
        new_status = "inherit";
        new_on_top = true;
        mfact = 0.618;
      };
      monitor = [
        ", preferred, auto, 1"
      ];
      # normal keybinds
      bind = let
        playerctl = "${pkgs.playerctl}/bin/playerctl";
        wl-copy = "${pkgs.wl-clipboard}/bin/wl-copy";
        systemctl = "${pkgs.systemd}/bin/systemctl";
      in [
        "$mod, 1, focusworkspaceoncurrentmonitor, 1"
        "$mod, 2, focusworkspaceoncurrentmonitor, 2"
        "$mod, 3, focusworkspaceoncurrentmonitor, 3"
        "$mod, 4, focusworkspaceoncurrentmonitor, 4"
        "$mod, 5, focusworkspaceoncurrentmonitor, 5"
        "$mod, 6, focusworkspaceoncurrentmonitor, 6"
        "$mod, 7, focusworkspaceoncurrentmonitor, 7"
        "$mod, 8, focusworkspaceoncurrentmonitor, 8"
        "$mod, 9, focusworkspaceoncurrentmonitor, 9"

        "$mod+SHIFT, 1, movetoworkspacesilent, 1"
        "$mod+SHIFT, 2, movetoworkspacesilent, 2"
        "$mod+SHIFT, 3, movetoworkspacesilent, 3"
        "$mod+SHIFT, 4, movetoworkspacesilent, 4"
        "$mod+SHIFT, 5, movetoworkspacesilent, 5"
        "$mod+SHIFT, 6, movetoworkspacesilent, 6"
        "$mod+SHIFT, 7, movetoworkspacesilent, 7"
        "$mod+SHIFT, 8, movetoworkspacesilent, 8"
        "$mod+SHIFT, 9, movetoworkspacesilent, 9"

        "$mod+ALT, 1, focusmonitor, 0"
        "$mod+ALT, 2, focusmonitor, 1"
        "$mod+ALT, 3, focusmonitor, 2"
        "$mod+ALT, 4, focusmonitor, 3"
        "$mod+ALT, 5, focusmonitor, 4"
        "$mod+ALT, 6, focusmonitor, 5"
        "$mod+ALT, 7, focusmonitor, 6"
        "$mod+ALT, 8, focusmonitor, 7"
        "$mod+ALT, 9, focusmonitor, 8"

        "$mod, q, exec, kitty -1"
        "$mod, d, exec, ${pkgs.fuzzel}/bin/fuzzel"

        "$mod, Print, exec, ${pkgs.grim}/bin/grim - | ${wl-copy}"
        "$mod+SHIFT, Print, exec, ${pkgs.slurp}/bin/slurp | ${pkgs.grim}/bin/grim -g - - | ${wl-copy}"
        "$mod+SHIFT, p, exec, ${pkgs.hyprpicker}/bin/hyprpicker | ${wl-copy}"
        "$mod+SHIFT, l, exec, ${pkgs.hyprlock}/bin/hyprlock"
        "$mod+SHIFT, w, exec, ${wallpaper-script}"
        "$mod+SHIFT, a, exec, ${pkgs.writeScript "toggle-activate-linux"
          # bash
          ''
            #!/bin/sh
            test "$(${systemctl} --user is-active activate-linux.service)" = "active" \
              && ${systemctl} --user stop activate-linux.service \
              || ${systemctl} --user start activate-linux.service
          ''}"

        ", XF86AudioPrev, exec, ${playerctl} previous"
        ", XF86AudioPlay, exec, ${playerctl} play-pause"
        ", XF86AudioNext, exec, ${playerctl} next"

        "$mod, c, killactive"
        "$mod+SHIFT, f, togglefloating"
        "$mod, f, fullscreen"
        "$mod, Tab, exec, ${pkgs.writeScript "hyprland-dropterm"
          # bash
          ''
            #!/bin/sh
            TERM_FILE="$XDG_RUNTIME_DIR/dropterm.pid"
            if [ -f "$TERM_FILE" ] ; then
              hyprctl dispatch togglespecialworkspace
            else
              echo $$ > "$TERM_FILE"
              trap 'rm -f $TERM_FILE' EXIT
              export SHLVL=0
              kitty --class dropterm
            fi
          ''}"

        "$mod, left, movefocus, l"
        "$mod, right, movefocus, r"
        "$mod, up, movefocus, u"
        "$mod, down, movefocus, d"

        "$mod, h, movefocus, l"
        "$mod, j, movefocus, d"
        "$mod, k, movefocus, u"
        "$mod, l, movefocus, r"

        "$mod+SHIFT, left, movewindow, l"
        "$mod+SHIFT, right, movewindow, r"
        "$mod+SHIFT, up, movewindow, u"
        "$mod+SHIFT, down, movewindow, d"

        "$mod+SHIFT, h, movewindow, l"
        "$mod+SHIFT, j, movewindow, d"
        "$mod+SHIFT, k, movewindow, u"
        "$mod+SHIFT, l, movewindow, r"

        "$mod, m, layoutmsg, rollnext"
        "$mod, n, layoutmsg, rollprev"
      ];
      # keybinds that'll repeat if held
      binde = let
        wpctl = "${pkgs.wireplumber}/bin/wpctl";
      in [
        ", XF86AudioLowerVolume, exec, ${pkgs.writeScript "vol-down"
          # python
          ''
            #!${pkgs.python3}/bin/python3

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
          ''}"
        ", XF86AudioRaiseVolume, exec, ${pkgs.writeScript "vol-up"
          # python
          ''
            #!${pkgs.python3}/bin/python3

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
          ''}"
        "SHIFT, XF86AudioLowerVolume, exec, ${wpctl} set-volume @DEFAULT_AUDIO_SINK@ 1%-"
        "SHIFT, XF86AudioRaiseVolume, exec, ${wpctl} set-volume -l 1.5 @DEFAULT_AUDIO_SINK@ 1%+"
        ", XF86MonBrightnessDown, exec, ${pkgs.brightnessctl}/bin/brightnessctl set 5%-"
        ", XF86MonBrightnessUp, exec, ${pkgs.brightnessctl}/bin/brightnessctl set 5%+"
      ];
      # keybinds that can be invoked even while locked
      bindl = [
        ", XF86AudioMute, exec, ${pkgs.wireplumber}/bin/wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"
      ];
      # mouse bindings
      bindm = [
        "$mod, mouse:272, movewindow"
        "$mod, mouse:273, resizewindow"
      ];
      exec = [
        "${pkgs.xorg.xhost}/bin/xhost +SI:localuser:root"
      ];
      exec-once = [
        "${pkgs.brightnessctl}/bin/brightnessctl set 75%"
      ];
      windowrulev2 = [
        "workspace special, class:^(dropterm)$"
      ];
    };
  };

  programs.hyprlock = {
    enable = true;
    settings = {
      general = {
        disable_loading_bar = true;
        grace = 0;
        hide_cursor = true;
        no_fade_in = false;
      };

      background = [
        {
          path = "screenshot";
          blur_passes = 3;
          blur_size = 4;
        }
      ];

      input-field = [
        {
          monitor = "";
          size = "1200, 200";
          check_color = "rgb(cd00f0)";
          dots_center = true;
          fade_on_empty = false;
          fail_color = "rgb(ff1212)";
          fail_text = "!! Authroization Failed !!";
          fail_timeout = 2000;
          font_color = "rgb(ff006e)";
          inner_color = "rgba(100c00f2)";
          outer_color = "rgb(ff006e)";
          outline_thickness = 3;
          placeholder_text = "> Authorization Required <";
          position = "0, 0";
          rounding = -1;
        }
      ];
    };
  };

  systemd.user = {
    services = {
      activate-linux = {
        Unit = {
          Description = "Activate Linux";
          Wants = ["hyprland-session.target"];
          After = ["hyprland-session.target"];
        };

        Service = {
          Type = "simple";
          ExecStart = "${pkgs.activate-linux}/bin/activate-linux";
        };

        Install = {
          WantedBy = ["hyprland-session.target"];
        };
      };
      hyprlock = {
        Unit = {
          Description = "hyprlock under xss-lock";
          Wants = ["hyprland-session.target"];
          After = ["hyprland-session.target"];
        };

        Service = {
          Type = "simple";
          ExecStart = "${pkgs.xss-lock}/bin/xss-lock ${pkgs.hyprlock}/bin/hyprlock";
        };

        Install = {
          WantedBy = ["hyprland-session.target"];
        };
      };
      swww-daemon = {
        Unit = {
          Description = "swww wallpaper daemon";
          Wants = ["hyprland-session.target"];
          After = ["hyprland-session.target"];
        };

        Service = {
          Type = "simple";
          ExecStart = "${pkgs.swww}/bin/swww-daemon";
          Restart = "on-failure";
          RestartSec = 1;
          TimeoutSec = "30s";
        };

        Install = {
          WantedBy = ["hyprland-session.target"];
        };
      };
      swww = {
        Unit = {
          Description = "cycle swww wallpaper";
          Wants = ["hyprland-session.target"];
          After = [
            "hyprland-session.target"
            "swww-daemon.service"
          ];
        };

        Service = {
          Type = "oneshot";
          ExecStart = "${wallpaper-script}";
        };

        Install = {
          WantedBy = ["hyprland-session.target"];
        };
      };
    };
    timers = {
      swww = {
        Unit = {
          Description = "cycle swww wallpaper";
          Wants = ["hyprland-session.target"];
          After = [
            "hyprland-session.target"
            "swww-daemon.service"
          ];
        };

        Timer = {
          OnUnitActiveSec = "5min";
          RandomizedDelaySec = "30s";
          AccuracySec = "30s";
        };

        Install = {
          WantedBy = ["hyprland-session.target"];
        };
      };
    };
  };
}
