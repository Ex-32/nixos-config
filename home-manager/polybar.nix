{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  services.polybar = {
    enable = true;
    package = pkgs.polybarFull;
    script = "polybar main &";
    config = {
      colors = {
        alert = "#ff1212";
        background = "#100c00";
        background-alt = "#252726";
        disabled = "#4a434a";
        foreground = "#bdadb8";
        primary = "#ff006e";
        secondary = "#ace700";
      };
      "bar/main" = {
        background = "\${colors.background}";
        border-color = "#00000000";
        border-size = "5pt";
        cursor-click = "pointer";
        cursor-scroll = "ns-resize";
        enable-ipc = "true";
        font-0 = "FiraCode Nerd Font:size=18;4";
        foreground = "\${colors.foreground}";
        height = "32pt";
        line-size = "3pt";
        module-margin = "1";
        modules-left = "xworkspaces xwindow";
        modules-right = "pulseaudio memory cpu wlan eth date systray";
        padding-left = "0";
        padding-right = "1";
        radius = "6";
        separator = "|";
        separator-foreground = "\${colors.disabled}";
        width = "100%";
        wm-restack = "generic";
      };

      "module/cpu" = {
        format-prefix = "CPU";
        format-prefix-foreground = "\${colors.primary}";
        interval = "2";
        label = "%percentage:2%%";
        type = "internal/cpu";
      };
      "module/date" = {
        date = "%H:%M";
        date-alt = "%Y-%m-%d %H:%M:%S";
        interval = "1";
        label = "%date%";
        label-foreground = "\${colors.primary}";
        type = "internal/date";
      };
      "module/eth" = {
        "inherit" = "network-base";
        interface-type = "wired";
        label-connected = "%{F#ff006e}%ifname%%{F-} %local_ip%";
      };
      "module/filesystem" = {
        interval = "25";
        label-mounted = "%{F#ff006e}%mountpoint%%{F-} %percentage_used%%";
        label-unmounted = "%mountpoint% not mounted";
        label-unmounted-foreground = "\${colors.disabled}";
        mount-0 = "/";
        type = "internal/fs";
      };
      "module/memory" = {
        format-prefix = "RAM";
        format-prefix-foreground = "\${colors.primary}";
        interval = "2";
        label = "%percentage_used:2%%";
        type = "internal/memory";
      };
      "module/pulseaudio" = {
        format-volume = "<label-volume>";
        format-volume-prefix = "VOL";
        format-volume-prefix-foreground = "\${colors.primary}";
        label-muted = "muted";
        label-muted-foreground = "\${colors.disabled}";
        label-volume = "%percentage%%";
        type = "internal/pulseaudio";
      };
      "module/systray" = {
        format-margin = "8pt";
        tray-spacing = "8pt";
        type = "internal/tray";
      };
      "module/wlan" = {
        "inherit" = "network-base";
        interface-type = "wireless";
        label-connected = "%{F#ff006e}%ifname%%{F-} %local_ip%";
      };
      "module/xwindow" = {
        label = "%title:0:60:...%";
        type = "internal/xwindow";
      };
      "module/xworkspaces" = {
        label-active = "%name%";
        label-active-background = "\${colors.background-alt}";
        label-active-padding = "1";
        label-active-underline = "\${colors.primary}";
        label-empty = "%name%";
        label-empty-foreground = "\${colors.disabled}";
        label-empty-padding = "1";
        label-occupied = "%name%";
        label-occupied-padding = "1";
        label-urgent = "%name%";
        label-urgent-background = "\${colors.alert}";
        label-urgent-padding = "1";
        type = "internal/xworkspaces";
      };
      network-base = {
        format-connected = "<label-connected>";
        format-disconnected = "<label-disconnected>";
        interval = "5";
        label-disconnected = "%{F#ff006e}%ifname%%{F#4a434a} disconnected";
        type = "internal/network";
      };
      settings = {
        pseudo-transparency = "true";
        screenchange-reload = "true";
      };
    };
  };
}
