{ config, pkgs, lib, inputs, ... }:

{
  imports = [
    ./gtk.nix
    ./mako.nix
    ./qt.nix
    ./swayidle.nix
    ./swaylock.nix
    ./systray.nix
    ./waybar.nix
  ];

  home.packages = with pkgs; [
    wl-clipboard
    wl-mirror
    xdg-desktop-portal-wlr
  ];

  wayland.windowManager.sway = {
    enable = true;
    extraConfigEarly = ''
      set $rosewater #f5e0dc
      set $flamingo  #f2cdcd
      set $pink      #f5c2e7
      set $mauve     #cba6f7
      set $red       #f38ba8
      set $maroon    #eba0ac
      set $peach     #fab387
      set $green     #a6e3a1
      set $teal      #94e2d5
      set $sky       #89dceb
      set $sapphire  #74c7ec
      set $blue      #89b4fa
      set $lavender  #b4befe
      set $text      #cdd6f4
      set $subtext1  #bac2de
      set $subtext0  #a6adc8
      set $overlay2  #9399b2
      set $overlay1  #7f849c
      set $overlay0  #6c7086
      set $surface2  #585b70
      set $surface1  #45475a
      set $surface0  #313244
      set $base      #1e1e2e
      set $mantle    #181825
      set $crust     #11111b  
    ''; # catppuccin-mocha colors
    config = rec {
      modifier = "Mod4";
      terminal = "wezterm";
      gaps = {
        inner = 0;
        outer = 0;
      };
      colors = {
        focused = {
          border = "$mauve";
          background = "$base";
          text = "$text";
          indicator = "$mauve";
          childBorder = "$mauve";
        };
        focusedInactive = {
          border = "$overlay0";
          background = "$base";
          text = "$text";
          indicator = "$mauve";
          childBorder = "$overlay0";
        };
        unfocused = {
          border = "$overlay0";
          background = "$base";
          text = "$text";
          indicator = "$mauve";
          childBorder = "$overlay0";
        };
        urgent = {
          border = "$red";
          background = "$base";
          text = "$red";
          indicator = "$overlay0";
          childBorder = "$red";
        };
        placeholder = {
          border = "$overlay0";
          background = "$base";
          text = "$text";
          indicator = "$overlay0";
          childBorder = "$overlay0";
        };
        background = "$crust";
      };
      output."*" = let
        wallpaper-pkg = inputs.nix-wallpaper.packages."x86_64-linux".default.override {
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
      keybindings = lib.mkOptionDefault {
        "${modifier}+Shift+q" = null;
        "${modifier}+Return" = null;
        "${modifier}+c" = "kill";
        "${modifier}+q" = "exec ${terminal}";
        "${modifier}+d" = ''
          exec ${pkgs.rofi-wayland}/bin/rofi \
            -show drun \
            -modi drun \
            -scroll-method 0 \
            -drun-match-fields all \
            -drun-display-format "{name}" \
            -no-drun-show-actions \
            -terminal wezterm \
            -theme ~/.config/rofi/config/launcher.rasi
        '';
        "${modifier}+Semicolon" = "exec ${pkgs.swaylock}/bin/swaylock";
        "XF86AudioMute" = "exec ${pkgs.wireplumber}/bin/wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle";
        "XF86AudioLowerVolume" = "exec ${pkgs.wireplumber}/bin/wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-";
        "XF86AudioRaiseVolume" = "exec ${pkgs.wireplumber}/bin/wpctl set-volume -l 1.5 @DEFAULT_AUDIO_SINK@ 5%+";
        "XF86AudioPrev" = "exec ${pkgs.playerctl}/bin/playerctl previous";
        "XF86AudioPlay" = "exec ${pkgs.playerctl}/bin/playerctl play-pause";
        "XF86AudioNext" = "exec ${pkgs.playerctl}/bin/playerctl next";
        "XF86MonBrightnessDown" = "exec ${pkgs.brightnessctl}/bin/brightnessctl set 5%-";
        "XF86MonBrightnessUp" = "exec ${pkgs.brightnessctl}/bin/brightnessctl set 5%+";
        "${modifier}+Print" = "exec sh -c '${pkgs.grim}/bin/grim - | ${pkgs.wl-clipboard}/bin/wl-copy'";
        "${modifier}+Shift+Print" = "exec sh -c '${pkgs.slurp}/bin/slurp | ${pkgs.grim}/bin/grim -g - - | ${pkgs.wl-clipboard}/bin/wl-copy'";
        "${modifier}+Tab" = "exec ~/.config/sway/bin/dropterm.sh"; # TODO make this more better
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
      ];
      window = {
        titlebar = false;
        border = 1;
      };
    };
  };

  home.file.".config/sway/bin/dropterm.sh" = {
    text = ''
      #!/bin/sh
      TERM_PIDFILE="''${XDG_RUNTIME_DIR:-~}/dropdown.pid"
      TERM_PID="$(cat "$TERM_PIDFILE")"
      if swaymsg "[ pid=$TERM_PID ] scratchpad show"; then
          # If multi-monitor configuration: resize on each monitor
          swaymsg "[ pid=$TERM_PID ] resize set 90ppt 90ppt, move position 5ppt 5ppt"
      else
          wezterm start --always-new-process &
          TERM_PID="$!"
          echo "$TERM_PID" > "$TERM_PIDFILE"
          swaymsg "for_window [ pid=$TERM_PID ] 'border pixel 1 ; floating enable ; resize set 90ppt 90ppt ; move position 5ppt 5ppt ; move to scratchpad ; scratchpad show'"
          trap 'kill $(jobs -p); rm -f "$TERM_PIDFILE"' EXIT
          wait "$TERM_PID"
      fi
    '';
    executable = true;
  };
}
