{ config, pkgs, lib, inputs, ... }:

{
  imports = [
    ./gtk.nix
    ./qt.nix
    ./swayidle.nix
    ./swaylock.nix
    ./systray.nix
    ./waybar.nix
    ./wezterm.nix
  ];

  wayland.windowManager.hyprland = {
    enable = true;
    systemd.enable = true;
    settings = {
      "$mod" = "SUPER";
      "$term" = "wezterm";
      general = {
        gaps_in = 3;
        gaps_out = 6;
        border_size = 1;
        "col.active_border" = "rgba(cba6f7ff) rgba(74c7ecff) 45deg";
        "col.inactive_border" = "rgba(313244ff)";
      };
      input = {
        kb_layout = "us";
        kb_options = "compose:ralt";
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
        blur = {
          enabled = false;
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
        force_hypr_chan = true;
      };
      dwindle = {
        special_scale_factor = 0.9;
      };
      master = {
        special_scale_factor = 0.9;
      };
      monitor = [
        ", preferred, auto, 1"
      ];
      # normal keybinds
      bind = [
        "$mod, 1, workspace, 1"
        "$mod, 2, workspace, 2"
        "$mod, 3, workspace, 3"
        "$mod, 4, workspace, 4"
        "$mod, 5, workspace, 5"
        "$mod, 6, workspace, 6"
        "$mod, 7, workspace, 7"
        "$mod, 8, workspace, 8"
        "$mod, 9, workspace, 9"

        "$mod SHIFT, 1, movetoworkspace, 1"
        "$mod SHIFT, 2, movetoworkspace, 2"
        "$mod SHIFT, 3, movetoworkspace, 3"
        "$mod SHIFT, 4, movetoworkspace, 4"
        "$mod SHIFT, 5, movetoworkspace, 5"
        "$mod SHIFT, 6, movetoworkspace, 6"
        "$mod SHIFT, 7, movetoworkspace, 7"
        "$mod SHIFT, 8, movetoworkspace, 8"
        "$mod SHIFT, 9, movetoworkspace, 9"

        "$mod, q, exec, $term"
        ''$mod, d, exec, ${pkgs.rofi-wayland}/bin/rofi -show drun -modi drun -scroll-method 0 -drun-match-fields all -drun-display-format "{name}" -no-drun-show-actions -terminal $term -theme config''
        "$mod, Semicolon, exec, swaylock"
        "$mod, Print, exec, sh -c '${pkgs.grim}/bin/grim - | ${pkgs.wl-clipboard}/bin/wl-copy'"
        "$mod SHIFT, Print, exec, sh -c '${pkgs.slurp}/bin/slurp | ${pkgs.grim}/bin/grim -g - - | ${pkgs.wl-clipboard}/bin/wl-copy'"

        ", XF86AudioPrev, exec, ${pkgs.playerctl}/bin/playerctl previous"
        ", XF86AudioPlay, exec, ${pkgs.playerctl}/bin/playerctl play-pause"
        ", XF86AudioNext, exec, ${pkgs.playerctl}/bin/playerctl next"

        "$mod, c, killactive"
        "$mod, Tab, togglespecialworkspace"
        "$mod, f, togglefloating"
        "$mod SHIFT, f, fullscreen"

        "$mod, left, movefocus, l"
        "$mod, right, movefocus, r"
        "$mod, up, movefocus, u"
        "$mod, down, movefocus, d"

        "$mod, h, movefocus, l"
        "$mod, j, movefocus, d"
        "$mod, k, movefocus, u"
        "$mod, l, movefocus, r"

        "$mod SHIFT, left, movewindow, l"
        "$mod SHIFT, right, movewindow, r"
        "$mod SHIFT, up, movewindow, u"
        "$mod SHIFT, down, movewindow, d"

        "$mod SHIFT, h, movewindow, l"
        "$mod SHIFT, j, movewindow, d"
        "$mod SHIFT, k, movewindow, u"
        "$mod SHIFT, l, movewindow, r"
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
        "[workspace special] wezterm start --always-new-process sh -c 'while :; do $SHELL; hyprctl dispatch togglespecialworkspace; clear; done'"
      ];
    };
  };

  home.file = {
    ".config/rofi/config.rasi".source = ../config/rofi/config.rasi;
  };
}
