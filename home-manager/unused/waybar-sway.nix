{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  imports = [
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
      target = "sway-session.target";
    };
    settings = [
      {
        "layer" = "top";
        "position" = "top";
        "modules-left" = [
          "sway/mode"
          "sway/workspaces"
          "custom/right-arrow-dark"
          "custom/right-arrow-light"
          "tray"
          "custom/right-arrow-dark"
          "custom/right-arrow-light"
          "idle_inhibitor"
          "custom/right-arrow-dark"
          "custom/right-arrow-light"
          "wireplumber"
          "custom/right-arrow-dark"
          "custom/right-arrow-light"
          "backlight"
          "custom/right-arrow-dark"
        ];
        "modules-center" = [
          "custom/left-arrow-dark"
          "clock#1"
          "custom/left-arrow-light"
          "custom/left-arrow-dark"
          "sway/window"
          "custom/right-arrow-dark"
          "custom/right-arrow-light"
          "clock#2"
          "custom/right-arrow-dark"
        ];
        "modules-right" = [
          "custom/left-arrow-dark"
          "battery"
          "custom/left-arrow-light"
          "custom/left-arrow-dark"
          "custom/ps"
          "custom/left-arrow-light"
          "custom/left-arrow-dark"
          "cpu"
          "custom/left-arrow-light"
          "custom/left-arrow-dark"
          "memory"
          "custom/left-arrow-light"
          "custom/left-arrow-dark"
          "disk"
        ];
        "custom/left-arrow-dark" = {
          "format" = "";
          "tooltip" = false;
        };
        "custom/left-arrow-light" = {
          "format" = "";
          "tooltip" = false;
        };
        "custom/right-arrow-dark" = {
          "format" = "";
          "tooltip" = false;
        };
        "custom/right-arrow-light" = {
          "format" = "";
          "tooltip" = false;
        };
        "sway/workspaces" = {
          "disable-scroll" = true;
          "format" = "{name}";
        };
        "clock#1" = {
          "format" = "{:%m/%d}";
          "tooltip" = false;
        };
        "clock#2" = {
          "format" = "{:%H:%M}";
          "tooltip" = false;
        };
        "clock#3" = {
          "format" = "{:%m-%d}";
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
        "sway/window" = {
          "max-length" = 50;
        };
        "idle_inhibitor" = {
          "format" = "{icon}";
          "format-icons" = {
            "activated" = "󰅶";
            "deactivated" = "󰾪";
          };
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
          font-family: 'FiraCode Nerd Font', monospace;
        }

        window#waybar {
          background: @crust;
          color: @text;
        }

        #custom-right-arrow-dark,
        #custom-left-arrow-dark {
          color: @base;
        }
        #custom-right-arrow-light,
        #custom-left-arrow-light {
          color: @crust;
          background: @base;
        }

        #workspaces,
        #mode,
        #clock.1,
        #clock.2,
        #window,
        #wireplumber,
        #backlight,
        #memory,
        #cpu,
        #battery,
        #disk,
        #tray,
        #idle_inhibitor,
        #custom-ps {
          background: @base;
        }

        #mode {
           color: @red;
        }
        #workspaces button {
          padding: 0 2px;
          color: @text;
        }
        #workspaces button.focused,
        #workspaces button.active {
          color: @mauve;
        }
        #workspaces button:hover {
          box-shadow: inherit;
          text-shadow: inherit;
        }
        #workspaces button:hover {
          background: @base;
          border: @base;
        }

        #wireplumber {
          color: @sapphire;
        }
        #wireplumber.muted {
          color: @surface1;
        }
        #memory {
          color: @blue;
        }
        #cpu {
          color: @lavender;
        }
        #battery.good {
          color: @green;
        }
        #battery.warning {
          color: @yellow;
        }
        #battery.critical {
          color: @red;
        }
        #disk {
          color: @peach;
        }
        #backlight {
          color: @rosewater;
        }
        #custom-ps {
          color: @pink;
        }

        #clock,
        #wireplumber,
        #backlight,
        #memory,
        #cpu,
        #battery,
        #disk,
        #custom-ps {
          padding: 0px 10px;
        }

        #tray {
          padding: 0px 0px 0px 6px;
        }

        #idle_inhibitor {
          padding: 0px 14px 0px 10px;
        }
        #idle_inhibitor.activated {
          color: @red;
        }
      '';
  };
}
