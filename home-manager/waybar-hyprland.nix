{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  imports = [
    ./nerdfont.nix
    ./systray.nix
  ];

  home.file.".config/waybar/bin/custom-ps.sh" = {
    text =
      /*
      bash
      */
      ''
        #!/bin/sh
        ${pkgs.procps}/bin/ps --ppid 2 -p 2 --deselect --no-headers | \
          ${pkgs.coreutils}/bin/wc -l
      '';
    executable = true;
  };

  programs.waybar = {
    enable = true;
    systemd = {
      enable = true;
      target = "hyprland-session.target";
    };
    settings = [
      {
        "layer" = "top";
        "position" = "top";
        "modules-left" = [
          "hyprland/workspaces"
          "idle_inhibitor"
          "hyprland/window"
        ];
        "modules-center" = [
          "systemd-failed-units"
        ];
        "modules-right" = [
          "wireplumber"
          "backlight"
          "battery"
          # "custom/ps"
          "cpu"
          "memory"
          # "disk"
          "clock"
          "tray"
        ];
        "hyprland/workspaces" = {
          "disable-scroll" = true;
          "format" = "{name}";
        };
        "clock" = {
          "format" = "{:%Y-%m-%d %H:%M}";
          "tooltip" = false;
        };
        "wireplumber" = {
          "format" = "{icon} {volume:2}%";
          "format-muted" = "󰖁 {volume}%";
          "format-icons" = [
            "󰕿"
            "󰖀"
            "󰕾"
            "󰕾"
          ];
          "scroll-step" = 5;
          "on-click" = "pamixer -t";
          "on-click-right" = "pavucontrol";
          "reverse-scrolling" = 1;
        };
        "backlight" = {
          "format" = "{icon} {percent}%";
          "format-icons" = [
            "󰃞"
            "󰃟"
            "󰃝"
            "󰃠"
          ];
        };
        "memory" = {
          "interval" = 5;
          "format" = "󰍛 {used}GiB";
        };
        "cpu" = {
          "interval" = 5;
          "format" = "󰘚 {usage}%";
        };
        "battery" = {
          "states" = {
            "good" = 100;
            "warning" = 30;
            "critical" = 15;
          };
          "format" = "{icon}  {capacity}%";
          "format-charging" = "󱐋 {capacity}%";
          "format-icons" = [
            ""
            ""
            ""
            ""
            ""
          ];
        };
        "disk" = {
          "interval" = 5;
          "format" = " {percentage_used:2}%";
          "path" = "/mnt/fsroot";
        };
        "tray" = {
          "icon-size" = 24;
        };
        "hyprland/window" = {
          "max-length" = 60;
        };
        "idle_inhibitor" = {
          "format" = "{icon}";
          "format-icons" = {
            "activated" = "󰅶";
            "deactivated" = "󰾪";
          };
        };
        "systemd-failed-units" = {
          "hide-on-ok" = true;
          "format" = "!! {nr_failed} failed units !!";
          "system" = true;
          "user" = true;
        };
        "custom/ps" = {
          "format" = "{icon} {}";
          "format-icons" = "";
          "exec" = "~/.config/waybar/bin/custom-ps.sh";
          "interval" = 20;
        };
      }
    ];
    style =
      /*
      css
      */
      ''
        @define-color base   #1e1e2e;
        @define-color mantle #181825;
        @define-color crust  #11111b;

        @define-color text     #cdd6f4;
        @define-color subtext0 #a6adc8;
        @define-color subtext1 #bac2de;

        @define-color surface0 #313244;
        @define-color surface1 #45475a;
        @define-color surface2 #585b70;

        @define-color overlay0 #6c7086;
        @define-color overlay1 #7f849c;
        @define-color overlay2 #9399b2;

        @define-color blue      #89b4fa;
        @define-color lavender  #b4befe;
        @define-color sapphire  #74c7ec;
        @define-color sky       #89dceb;
        @define-color teal      #94e2d5;
        @define-color green     #a6e3a1;
        @define-color yellow    #f9e2af;
        @define-color peach     #fab387;
        @define-color maroon    #eba0ac;
        @define-color red       #f38ba8;
        @define-color mauve     #cba6f7;
        @define-color pink      #f5c2e7;
        @define-color flamingo  #f2cdcd;
        @define-color rosewater #f5e0dc;

        * {
          font-size: 20px;
          font-family: "FiraCode Nerd Font", monospace;

          border: none;
          border-radius: 0;

          box-shadow: none;
          text-shadow: none;
        }

        window {
          background: linear-gradient(rgba(30, 30, 46, 0.8), rgba(30, 30, 46, 0));
          color: @text;
        }

        window#waybar.empty #window {
          background: rgba(0, 0, 0, 0);
          color: rgba(0, 0, 0, 0);
        }

        #workspaces,
        #clock,
        #window,
        #wireplumber,
        #backlight,
        #memory,
        #cpu,
        #battery,
        #disk,
        #tray,
        #idle_inhibitor,
        #systemd-failed-units,
        #custom-ps {
          background: @mantle;
          color: @text;
          margin: 0px 3px 0px 3px;
          min-width: 25px;
          border-radius: 20px;
          padding: 0px 10px 0px 10px;
        }

        #workspaces button {
          color: @text;
        }
        #workspaces button.focused,
        #workspaces button.active {
          color: @mauve;
        }
        #workspaces button.urgent {
          background: @red;
          color: @mantle;
        }
        #workspaces button:hover {
          background: @base;
          color: @sapphire;
        }

        #idle_inhibitor {
          padding: 0px 13px 0px 10px;
        }

        #idle_inhibitor.activated {
          background: @red;
          color: @mantle;
        }

        #systemd-failed-units {
          background: @red;
          color: @mantle;
        }

        #wireplumber {
          background: @sky;
          color: @mantle;
        }
        #wireplumber.muted {
          background: @mantle;
          color: @text;
        }

        #backlight {
          background: @rosewater;
          color: @mantle;
        }

        #battery {
          color: @mantle;
        }
        #battery.good {
          background: @green;
        }
        #battery.warning {
          background: @yellow;
        }
        #battery.critical {
          background: @red;
        }

        #cpu {
          background: @green;
          color: @mantle;
        }

        #memory {
          background: @sapphire;
          color: @mantle;
        }

        #clock {
          background: @lavender;
          color: @mantle;
        }
      '';
  };
}
