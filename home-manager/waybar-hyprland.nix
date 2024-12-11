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
      # bash
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
      # css
      ''
        @define-color text #fcfcfc;

        @define-color grey1 #100c00;
        @define-color grey2 #252726;
        @define-color grey3 #848484;
        @define-color grey4 #9e9e91;

        @define-color metal1 #4a434a;
        @define-color metal2 #9a8898;
        @define-color metal3 #bdadb8;

        @define-color green1 #00b300;
        @define-color green2 #00d500;
        @define-color green3 #82d700;
        @define-color green4 #ace700;
        @define-color green5 #d6e800;

        @define-color pink1 #ff006e;
        @define-color pink2 #ff2bff;
        @define-color pink3 #ff6eff;
        @define-color pink4 #ff85ac;
        @define-color pink5 #ff9ed9;

        @define-color red1 #9c000e;
        @define-color red2 #ff1212;
        @define-color red3 #fe552c;
        @define-color red4 #fff0bc;

        @define-color blue1 #3105b0;
        @define-color blue2 #4105fb;
        @define-color blue3 #7000fc;
        @define-color blue4 #9c00fc;
        @define-color blue5 #cd00f0;

        @define-color cyan1 #00b0df;
        @define-color cyan2 #00bbfe;
        @define-color cyan3 #01d2fd;
        @define-color cyan4 #25d0fe;
        @define-color cyan5 #abf9fd;

        @define-color mil1 #2b2e27;
        @define-color mil2 #3f423b;
        @define-color mil3 #6f7567;
        @define-color mil4 #8f9772;
        @define-color mil5 #afb48b;

        * {
          font-size: 21px;
          font-family: "FiraCode Nerd Font", monospace;

          border: none;
          border-radius: 0;

          box-shadow: none;
          text-shadow: none;
          background: rgba(0, 0, 0, 0);
        }

        window {
          background: linear-gradient(rgba(16, 12, 0, 0.8), rgba(16, 12, 0, 0));
          margin: 0px;
          padding: 0px;
        }

        window#waybar.empty #window {
          color: rgba(0, 0, 0, 0);
        }

        #tray menu {
          background: @grey2;
          color: @text;
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
          color: @text;
          margin: 0px 8px;
          min-width: 25px;
        }

        #workspaces button {
          color: @text;
        }
        #workspaces button.focused,
        #workspaces button.active {
          color: @pink2;
        }
        #workspaces button.urgent {
          color: @pink1;
        }
        #workspaces button:hover {
          color: @pink3;
        }

        #idle_inhibitor {
          color: @grey3;
        }
        #idle_inhibitor.activated {
          color: @pink1;
        }

        #systemd-failed-units {
          color: @red2;
        }

        #wireplumber {
          color: @cyan2;
        }
        #wireplumber.muted {
          color: @grey3;
        }

        #backlight {
          color: @red4;
        }

        #battery.good {
          color: @green2;
        }
        #battery.warning {
          color: @green5;
        }
        #battery.critical {
          color: @red2;
        }

        #cpu {
          color: @green4;
        }

        #memory {
          color: @cyan1;
        }

        #clock {
          color: @text;
        }
      '';
  };
}
