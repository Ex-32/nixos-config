{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  imports = [
    ./gtk.nix
    ./mako.nix
    ./qt.nix
    ./waybar.nix
  ];

  home.packages = with pkgs; [
    wl-clipboard
    wl-mirror
    xdg-desktop-portal
    xdg-desktop-portal-wlr
    xdg-desktop-portal-gtk
  ];

  wayland.windowManager.sway = let
    # currently, there is some friction between sway and gtk:
    # https://github.com/swaywm/sway/wiki/GTK-3-settings-on-Wayland
    # the suggested way to set gtk settings is with gsettings
    # for gsettings to work, we need to tell it where the schemas are
    # using the XDG_DATA_DIR environment variable
    # run at the end of sway config
    configure-gtk = pkgs.writeTextFile {
      name = "configure-gtk";
      destination = "/bin/configure-gtk";
      executable = true;
      text = let
        gesttings = "${pkgs.glib}/bin/gsettings";
        schema = pkgs.gsettings-desktop-schemas;
        datadir = "${schema}/share/gsettings-schemas/${schema.name}";
      in
        /*
        bash
        */
        ''
          export XDG_DATA_DIRS=${datadir}:$XDG_DATA_DIRS
          gnome_schema=org.gnome.desktop.interface
          ${gesttings} set $gnome_schema gtk-theme "${config.gtk.theme.name}"
          ${gesttings} set $gnome_schema cursor-theme "${config.gtk.cursorTheme.name}"
        '';
    };
  in {
    enable = true;
    package = pkgs.swayfx;
    wrapperFeatures.gtk = true;
    config = rec {
      modifier = "Mod4";
      terminal = "kitty";
      gaps = {
        inner = 8;
        outer = 0;
      };
      colors = let
        rosewater = "#f5e0dc";
        flamingo = "#f2cdcd";
        pink = "#f5c2e7";
        mauve = "#cba6f7";
        red = "#f38ba8";
        maroon = "#eba0ac";
        peach = "#fab387";
        green = "#a6e3a1";
        teal = "#94e2d5";
        sky = "#89dceb";
        sapphire = "#74c7ec";
        blue = "#89b4fa";
        lavender = "#b4befe";
        text = "#cdd6f4";
        subtext1 = "#bac2de";
        subtext0 = "#a6adc8";
        overlay2 = "#9399b2";
        overlay1 = "#7f849c";
        overlay0 = "#6c7086";
        surface2 = "#585b70";
        surface1 = "#45475a";
        surface0 = "#313244";
        base = "#1e1e2e";
        mantle = "#181825";
        crust = "#11111b";
      in {
        focused = {
          border = "${mauve}";
          background = "${base}";
          text = "${text}";
          indicator = "${mauve}";
          childBorder = "${mauve}";
        };
        focusedInactive = {
          border = "${overlay0}";
          background = "${base}";
          text = "${text}";
          indicator = "${mauve}";
          childBorder = "${overlay0}";
        };
        unfocused = {
          border = "${overlay0}";
          background = "${base}";
          text = "${text}";
          indicator = "${mauve}";
          childBorder = "${overlay0}";
        };
        urgent = {
          border = "${red}";
          background = "${base}";
          text = "${red}";
          indicator = "${overlay0}";
          childBorder = "${red}";
        };
        placeholder = {
          border = "${overlay0}";
          background = "${base}";
          text = "${text}";
          indicator = "${overlay0}";
          childBorder = "${overlay0}";
        };
        background = "${crust}";
      };
      seat."*" = {
        xcursor_theme = "${config.home.pointerCursor.name} ${toString config.home.pointerCursor.size}";
      };
      output."*" = let
        wallpaper-pkg = inputs.nix-wallpaper.packages."${pkgs.system}".default.override {
          preset = "catppuccin-mocha-rainbow";
          width = 3840;
          height = 2160;
          logoSize = 42;
          backgroundColor = "#11111b";
        };
      in {
        scale = "1";
        background = "${wallpaper-pkg}/share/wallpapers/nixos-wallpaper.png fill";
      };
      keybindings = let
        mod = "${modifier}";
        playerctl = "${pkgs.playerctl}/bin/playerctl";
        wpctl = "${pkgs.wireplumber}/bin/wpctl";
        brightnessctl = "${pkgs.brightnessctl}/bin/brightnessctl";
        wl-copy = "${pkgs.wl-clipboard}/bin/wl-copy";
      in
        lib.mkOptionDefault {
          "${mod}+Shift+q" = null;
          "${mod}+Return" = null;
          "${mod}+c" = "kill";
          "${mod}+q" = "exec ${terminal}";
          "${mod}+d" = ''
            exec ${pkgs.rofi-wayland}/bin/rofi \
              -show drun \
              -modi drun \
              -scroll-method 0 \
              -drun-match-fields all \
              -drun-display-format "{name}" \
              -no-drun-show-actions \
              -terminal ${terminal} \
              -theme config
          '';
          "${mod}+Shift+s" = "sticky toggle";
          "${mod}+Semicolon" = "exec ${pkgs.swaylock-effects}/bin/swaylock";
          "XF86AudioMute" = "exec ${wpctl} set-mute @DEFAULT_AUDIO_SINK@ toggle";
          "XF86AudioLowerVolume" = "exec ${wpctl} set-volume @DEFAULT_AUDIO_SINK@ 5%-";
          "XF86AudioRaiseVolume" = "exec ${wpctl} set-volume -l 1.5 @DEFAULT_AUDIO_SINK@ 5%+";
          "XF86AudioPrev" = "exec ${playerctl} previous";
          "XF86AudioPlay" = "exec ${playerctl} play-pause";
          "XF86AudioNext" = "exec ${playerctl} next";
          "XF86MonBrightnessDown" = "exec ${brightnessctl} set 5%-";
          "XF86MonBrightnessUp" = "exec ${brightnessctl} set 5%+";
          "${mod}+Print" = "exec sh -c '${pkgs.grim}/bin/grim - | ${wl-copy}'";
          "${mod}+Shift+Print" = "exec sh -c '${pkgs.slurp}/bin/slurp | ${pkgs.grim}/bin/grim -g - - | ${wl-copy}'";
          "${mod}+Shift+p" = "exec ${pkgs.hyprpicker}/bin/hyprpicker | ${wl-copy}";
          "${mod}+Tab" = "exec ~/.config/sway/bin/dropterm.sh"; # TODO: make this more better
        };
      input = {
        "type:touchpad" = {
          dwt = "disabled";
          natural_scroll = "enabled";
          click_method = "clickfinger";
        };
        "*" = {
          pointer_accel = "0.7";
          xkb_options = "compose:ralt";
        };
      };
      bars = [];
      startup = [
        {
          command = ''"${pkgs.xorg.xhost}/bin/xhost +SI:localuser:root"'';
          always = true;
        }
        {
          command = ''${pkgs.brightnessctl}/bin/brightnessctl set 75%'';
        }
        {
          command = ''${configure-gtk}/bin/configure-gtk'';
        }
      ];
      window = {
        titlebar = false;
        border = 1;
      };
    };
    # this config is specific to swayfx
    extraConfig = ''
      blur enable
      blur_xray disable
      blur_radius 2
      shadows enable
      default_dim_inactive 0.3
    '';
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
    ".config/sway/bin/dropterm.sh" = {
      text =
        /*
        bash
        */
        ''
          #!/bin/sh
          TERM_PIDFILE="''${XDG_RUNTIME_DIR}/dropdown.pid"
          TERM_PID="$(cat "$TERM_PIDFILE")"
          if swaymsg "[ pid=$TERM_PID ] scratchpad show"; then
              # If multi-monitor configuration: resize on each monitor
              swaymsg "[ pid=$TERM_PID ] resize set 90ppt 90ppt, move position 5ppt 5ppt"
          else
              kitty &
              TERM_PID="$!"
              echo "$TERM_PID" > "$TERM_PIDFILE"
              swaymsg "for_window [ pid=$TERM_PID ] '${lib.strings.concatStrings [
            "border pixel 1;"
            "floating enable;"
            "resize set 90ppt 90ppt;"
            "move position 5ppt 5ppt;"
            "move to scratchpad;"
            "scratchpad show"
          ]}'"
              trap 'kill $(jobs -p); rm -f "$TERM_PIDFILE"' EXIT
              wait "$TERM_PID"
          fi
        '';
      executable = true;
    };
  };
}
