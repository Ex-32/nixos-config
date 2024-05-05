{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  imports = [
    inputs.hyprland.homeManagerModules.default
    ./gtk.nix
    ./qt.nix
    ./systray.nix
    ./waybar.nix
    ./kitty.nix
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
    plugins = [
      inputs.hy3.packages.${pkgs.system}.hy3
    ];
    settings = {
      "$mod" = "SUPER";
      "$term" = "kitty";
      general = {
        gaps_in = 6;
        gaps_out = 12;
        border_size = 4;
        "col.active_border" = "rgb(${mauve}) rgb(${sapphire}) 45deg";
        "col.inactive_border" = "0xff${surface0}";
        layout = "hy3";
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
        force_default_wallpaper = 2;
      };
      dwindle = {
        special_scale_factor = 0.9;
      };
      master = {
        special_scale_factor = 0.9;
      };
      plugin = {
        hy3 = {
          tabs = {
            height = 4;
            padding = 6;
            render_text = false;

            "col.active" = "0xff${mauve}";
            "col.urgent" = "0xff${red}";
            "col.inactive" = "0xff${surface1}";
          };
        };
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

        "$mod+SHIFT, 1, hy3:movetoworkspace, 1"
        "$mod+SHIFT, 2, hy3:movetoworkspace, 2"
        "$mod+SHIFT, 3, hy3:movetoworkspace, 3"
        "$mod+SHIFT, 4, hy3:movetoworkspace, 4"
        "$mod+SHIFT, 5, hy3:movetoworkspace, 5"
        "$mod+SHIFT, 6, hy3:movetoworkspace, 6"
        "$mod+SHIFT, 7, hy3:movetoworkspace, 7"
        "$mod+SHIFT, 8, hy3:movetoworkspace, 8"
        "$mod+SHIFT, 9, hy3:movetoworkspace, 9"

        "$mod, q, exec, $term"
        ''$mod, d, exec, ${pkgs.rofi-wayland}/bin/rofi -show drun -modi drun -scroll-method 0 -drun-match-fields all -drun-display-format "{name}" -no-drun-show-actions -terminal $term -theme config''
        "$mod, Semicolon, exec, swaylock"
        "$mod, Print, exec, sh -c '${pkgs.grim}/bin/grim - | ${pkgs.wl-clipboard}/bin/wl-copy'"
        "$mod+SHIFT, Print, exec, sh -c '${pkgs.slurp}/bin/slurp | ${pkgs.grim}/bin/grim -g - - | ${pkgs.wl-clipboard}/bin/wl-copy'"

        ", XF86AudioPrev, exec, ${pkgs.playerctl}/bin/playerctl previous"
        ", XF86AudioPlay, exec, ${pkgs.playerctl}/bin/playerctl play-pause"
        ", XF86AudioNext, exec, ${pkgs.playerctl}/bin/playerctl next"

        "$mod, c, hy3:killactive"
        "$mod, Tab, togglespecialworkspace"
        "$mod+SHIFT, f, togglefloating"
        "$mod, f, fullscreen"

        "$mod, e, hy3:makegroup, h"
        "$mod, s, hy3:makegroup, v"
        "$mod, w, hy3:makegroup, tab"
        "$mod, a, hy3:changefocus, raise"
        "$mod+SHIFT, a, hy3:changefocus, lower"
        "$mod, r, hy3:changegroup, opposite"

        "$mod, left, hy3:movefocus, l"
        "$mod, right, hy3:movefocus, r"
        "$mod, up, hy3:movefocus, u"
        "$mod, down, hy3:movefocus, d"

        "$mod, h, hy3:movefocus, l"
        "$mod, j, hy3:movefocus, d"
        "$mod, k, hy3:movefocus, u"
        "$mod, l, hy3:movefocus, r"

        "$mod+SHIFT, left, hy3:movewindow, l, once"
        "$mod+SHIFT, right, hy3:movewindow, r, once"
        "$mod+SHIFT, up, hy3:movewindow, u, once"
        "$mod+SHIFT, down, hy3:movewindow, d, once"

        "$mod+SHIFT, h, hy3:movewindow, l, once"
        "$mod+SHIFT, j, hy3:movewindow, d, once"
        "$mod+SHIFT, k, hy3:movewindow, u, once"
        "$mod+SHIFT, l, hy3:movewindow, r, once"
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
      bindn = [
        ", mouse:272, hy3:focustab, mouse"
        ", mouse_down, hy3:focustab, l, require_hovered"
        ", mouse_up, hy3:focustab, r, require_hovered"
      ];
      exec = [
        "${pkgs.xorg.xhost}/bin/xhost +SI:localuser:root"
      ];
      exec-once = [
        "[workspace special] $term sh -c 'while :; do $SHELL; hyprctl dispatch togglespecialworkspace; clear; done'"
        "${pkgs.brightnessctl}/bin/brightnessctl set 75%"
      ];
    };
  };

  home.file = {
    ".config/rofi/config.rasi".source = ../config/rofi/config.rasi;
  };
}
