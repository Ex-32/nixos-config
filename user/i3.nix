{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  imports = [
    ./gtk.nix
    ./picom.nix
    ./qt.nix
    ./tint2.nix
  ];

  home.packages = with pkgs; [
    xclip
  ];

  xsession.enable = true;
  xsession.windowManager.i3 = {
    enable = true;
    config = rec {
      modifier = "Mod4";
      terminal = "${pkgs.wezterm}/bin/wezterm";
      gaps = {
        inner = 10;
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
      keybindings = lib.mkOptionDefault {
        "${modifier}+h" = "focus left";
        "${modifier}+j" = "focus down";
        "${modifier}+k" = "focus up";
        "${modifier}+l" = "focus right";
        "${modifier}+Left" = "focus left";
        "${modifier}+Down" = "focus down";
        "${modifier}+Up" = "focus up";
        "${modifier}+Right" = "focus right";

        "${modifier}+Shift+h" = "move left";
        "${modifier}+Shift+j" = "move down";
        "${modifier}+Shift+k" = "move up";
        "${modifier}+Shift+l" = "move right";
        "${modifier}+Shift+Left" = "move left";
        "${modifier}+Shift+Down" = "move down";
        "${modifier}+Shift+Up" = "move up";
        "${modifier}+Shift+Right" = "move right";

        "${modifier}+Shift+q" = null;
        "${modifier}+Return" = null;
        "${modifier}+r" = "mode resize";
        "${modifier}+c" = "kill";
        "${modifier}+q" = "exec ${terminal}";
        "${modifier}+d" = ''
          exec ${pkgs.rofi}/bin/rofi \
            -show drun \
            -modi drun \
            -scroll-method 0 \
            -drun-match-fields all \
            -drun-display-format "{name}" \
            -no-drun-show-actions \
            -terminal ${terminal} \
            -theme config
        '';
        "${modifier}+semicolon" = "exec --no-startup-id ${pkgs.i3}/bin/i3-nagbar -t warning -m 'no lockscreeen configured'";
        "XF86AudioMute" = "exec --no-startup-id ${pkgs.wireplumber}/bin/wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle";
        "XF86AudioLowerVolume" = "exec --no-startup-id ${pkgs.wireplumber}/bin/wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-";
        "XF86AudioRaiseVolume" = "exec --no-startup-id ${pkgs.wireplumber}/bin/wpctl set-volume -l 1.5 @DEFAULT_AUDIO_SINK@ 5%+";
        "XF86AudioPrev" = "exec --no-startup-id ${pkgs.playerctl}/bin/playerctl previous";
        "XF86AudioPlay" = "exec --no-startup-id ${pkgs.playerctl}/bin/playerctl play-pause";
        "XF86AudioNext" = "exec --no-startup-id ${pkgs.playerctl}/bin/playerctl next";
        "XF86MonBrightnessDown" = "exec --no-startup-id ${pkgs.brightnessctl}/bin/brightnessctl set 5%-";
        "XF86MonBrightnessUp" = "exec --no-startup-id ${pkgs.brightnessctl}/bin/brightnessctl set 5%+";
        "${modifier}+Print" = "exec --no-startup-id sh -c '${pkgs.scrot}/bin/scrot - | ${pkgs.xclip}/bin/xclip -selection clipboard -target image/png -i'";
        "${modifier}+Shift+Print" = "--release exec --no-startup-id sh -c '${pkgs.scrot}/bin/scrot -s - | ${pkgs.xclip}/bin/xclip -selection clipboard -target image/png -i'";
        "${modifier}+Tab" = "exec --no-startup-id ${pkgs.tdrop}/bin/tdrop -h 82% -w 88% -x 6% -y 9% wezterm start --always-new-process --class floating";
      };
      modes.resize = {
        "Left" = "resize shrink width 10 px or 10 ppt";
        "Down" = "resize grow height 10 px or 10 ppt";
        "Up" = "resize shrink height 10 px or 10 ppt";
        "Right" = "resize grow width 10 px or 10 ppt";
        "h" = "resize shrink width 10 px or 10 ppt";
        "j" = "resize grow height 10 px or 10 ppt";
        "k" = "resize shrink height 10 px or 10 ppt";
        "l" = "resize grow width 10 px or 10 ppt";
        "Escape" = "mode default";
        "Return" = "mode default";
        "${modifier}+r" = "mode default";
      };
      bars = [];
      startup = let
        wallpaper-pkg = inputs.nix-wallpaper.packages."${pkgs.system}".default.override {
          preset = "catppuccin-mocha-rainbow";
          width = 3840;
          height = 2160;
          logoSize = 42;
          backgroundColor = "#11111b";
        };
      in [
        {
          command = ''"${pkgs.xorg.xhost}/bin/xhost +SI:localuser:root"'';
          always = true;
          notification = false;
        }
        {
          command = ''"${pkgs.feh}/bin/feh --no-fehbg --bg-fill ${wallpaper-pkg}/share/wallpapers/nixos-wallpaper.png"'';
          always = true;
          notification = false;
        }
      ];
      window = {
        titlebar = false;
        border = 1;
      };
      floating.criteria = [
        {class = "^floating$";}
      ];
    };
  };

  home.file.".config/rofi/config.rasi".source = ../config/rofi/config.rasi;
}
