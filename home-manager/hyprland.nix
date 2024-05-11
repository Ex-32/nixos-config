{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  imports = [
    ./gtk.nix
    ./qt.nix
    ./systray.nix
    ./waybar-hyprland.nix
    ./kitty.nix
    ./mako.nix
  ];

  home.packages = with pkgs; [
    wl-clipboard
    wl-mirror
    xdg-desktop-portal
    xdg-desktop-portal-gtk
    xdg-desktop-portal-hyprland
  ];

  wayland.windowManager.hyprland = let
    rosewater = "f5e0dc";
    flamingo = "f2cdcd";
    pink = "f5c2e7";
    mauve = "cba6f7";
    red = "f38ba8";
    maroon = "eba0ac";
    peach = "fab387";
    green = "a6e3a1";
    teal = "94e2d5";
    sky = "89dceb";
    sapphire = "74c7ec";
    blue = "89b4fa";
    lavender = "b4befe";
    text = "cdd6f4";
    subtext1 = "bac2de";
    subtext0 = "a6adc8";
    overlay2 = "9399b2";
    overlay1 = "7f849c";
    overlay0 = "6c7086";
    surface2 = "585b70";
    surface1 = "45475a";
    surface0 = "313244";
    base = "1e1e2e";
    mantle = "181825";
    crust = "11111b";
  in {
    enable = true;
    systemd.enable = true;
    settings = {
      "$mod" = "SUPER";
      general = {
        gaps_in = 6;
        gaps_out = 12;
        border_size = 4;
        "col.active_border" = "rgb(${mauve}) rgb(${sapphire}) 45deg";
        "col.inactive_border" = "0xff${surface0}";
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
        rounding = 10;
        blur = {
          enabled = true;
          size = 2;
        };
        drop_shadow = false;
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
        new_is_master = false;
        new_on_top = true;
      };
      monitor = [
        ", preferred, auto, 1"
      ];
      # normal keybinds
      bind = let
        playerctl = "${pkgs.playerctl}/bin/playerctl";
        wl-copy = "${pkgs.wl-clipboard}/bin/wl-copy";
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
        ''$mod, d, exec, ${pkgs.rofi-wayland}/bin/rofi -show drun -modi drun -scroll-method 0 -drun-match-fields all -drun-display-format "{name}" -no-drun-show-actions -terminal "kitty -1" -theme config''
        "$mod, Semicolon, exec, swaylock"
        "$mod, Print, exec, ${pkgs.grim}/bin/grim - | ${wl-copy}"
        "$mod+SHIFT, Print, exec, ${pkgs.slurp}/bin/slurp | ${pkgs.grim}/bin/grim -g - - | ${wl-copy}"
        "$mod+SHIFT, p, exec, ${pkgs.hyprpicker}/bin/hyprpicker | ${wl-copy}"
        "$mod+SHIFT, w, exec, ${config.home.homeDirectory}/.config/hypr/bin/wallpaper.sh"

        ", XF86AudioPrev, exec, ${playerctl} previous"
        ", XF86AudioPlay, exec, ${playerctl} play-pause"
        ", XF86AudioNext, exec, ${playerctl} next"

        "$mod, c, killactive"
        "$mod, Tab, exec, ~/.config/hypr/bin/dropterm.sh"
        "$mod+SHIFT, f, togglefloating"
        "$mod, f, fullscreen"

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
      binde = [
        ", XF86AudioLowerVolume, exec, ${pkgs.wireplumber}/bin/wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-"
        ", XF86AudioRaiseVolume, exec, ${pkgs.wireplumber}/bin/wpctl set-volume -l 1.5 @DEFAULT_AUDIO_SINK@ 5%+"
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
      windowrule = [
        "workspace special, ^(dropterm)$"
      ];
    };
  };

  services.swayidle = {
    enable = true;
    timeouts = [
      {
        timeout = 180;
        command = "${pkgs.swaylock-effects}/bin/swaylock --grace 20";
      }
    ];
    events = [
      {
        event = "before-sleep";
        command = "${pkgs.swaylock-effects}/bin/swaylock";
      }
    ];
  };

  programs.swaylock = {
    enable = true;
    package = pkgs.swaylock-effects;
    settings = {
      screenshots = true;
      clock = true;
      timestr = "%H:%M:%S";
      datestr = "%Y-%m-%d";
      indicator = true;
      indicator-radius = 350;
      indicator-thickness = 12;
      effect-blur = "8x5";
      ring-color = "cba6f7";
      ring-clear-color = "fab387";
      ring-ver-color = "74c7ec";
      ring-wrong-color = "f38ba8";
      key-hl-color = "45475a";
      bs-hl-color = "fab387";
      line-color = "00000000";
      line-clear-color = "00000000";
      line-caps-lock-color = "00000000";
      line-ver-color = "00000000";
      line-wrong-color = "00000000";
      inside-color = "00000000";
      inside-clear-color = "00000000";
      inside-caps-lock-color = "00000000";
      inside-ver-color = "00000000";
      inside-wrong-color = "00000000";
      separator-color = "00000000";
      text-color = "cba6f7";
      text-clear-color = "fab387";
      text-caps-lock-color = "f38ba8";
      text-ver-color = "74c7ec";
      text-wrong-color = "f38ba8";
      fade-in = 0.2;
      font = "Raleway";
    };
  };

  home.file = {
    ".config/rofi/config.rasi".source = ../config/rofi/config.rasi;
    ".config/hypr/bin/dropterm.sh" = {
      executable = true;
      text =
        /*
        bash
        */
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
        '';
    };
    ".config/hypr/hyprpaper.conf".text = "splash = true";
    ".config/hypr/bin/wallpaper.sh" = {
      executable = true;
      text = let
        rand-file = "/dev/urandom";
        core = "${pkgs.coreutils}/bin";
      in
        /*
        bash
        */
        ''
          #!/bin/sh
          set -e
          # FIXME: don't hardcode wallpaper path
          WALLPAPER_PATH="${config.home.homeDirectory}/documents/pictures/wallpapers/current"
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
    };
  };

  systemd.user = {
    services = {
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
          ExecStart = "${config.home.homeDirectory}/.config/hypr/bin/wallpaper.sh";
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
