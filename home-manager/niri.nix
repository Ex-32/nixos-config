{
  config,
  osConfig,
  pkgs,
  lib,
  inputs,
  ...
}: let
  dash-shebang = "#!" + lib.getExe pkgs.dash;

  wallpaper-script = let
    core = builtins.listToAttrs (builtins.map (name: {
        inherit name;
        value = lib.getExe' pkgs.coreutils name;
      }) [
        "od"
        "ls"
        "printf"
        "shuf"
      ]);
    home = config.home.homeDirectory;
    hostname = osConfig.networking.hostName;
    rand-file = "/dev/urandom";
  in
    pkgs.writeScript "niri-wallpaper"
    # bash
    ''
      ${dash-shebang}
      set -e
      # FIXME: don't hardcode wallpaper path
      WALLPAPER_PATH="${home}/documents/pictures/wallpapers/${hostname}"
      if [ ! -d "$WALLPAPER_PATH" ] ; then
        echo "wallpaper directory '$WALLPAPER_PATH' doesn't exist"
        exit 1
      fi
      rand_pos() {
        ${core.printf} '0.%02d' $(( $(${core.od} -An -N2 -d ${rand-file}) % 99 + 1 ))
      }
      RAND_PAPER="$WALLPAPER_PATH/$(${core.ls} -1 "$WALLPAPER_PATH" |\
        ${core.shuf} --random-source=${rand-file} -n 1)"
      ${lib.getExe pkgs.swww} img \
        --transition-duration 2 \
        --transition-type grow \
        --transition-pos "$(rand_pos),$(rand_pos)" \
        "$RAND_PAPER"
    '';
in {
  imports = [
    ./gtk.nix
    ./kitty.nix
    ./mako.nix
    ./qt.nix
    ./waybar.nix
    ./fuzzel.nix
  ];

  xdg.configFile."niri/config.kdl".source = pkgs.replaceVars ../config/niri/config.kdl rec {
    # @variables@ to substitute

    ## packages
    brightnessctl = lib.getExe pkgs.brightnessctl;
    fuzzel = lib.getExe pkgs.fuzzel;
    kitty = lib.getExe pkgs.kitty;
    playerctl = lib.getExe pkgs.playerctl;
    systemctl = lib.getExe' pkgs.systemd "systemctl";
    wpctl = lib.getExe' pkgs.wireplumber "wpctl";

    ## scripts
    startup_hook = let
      variables = builtins.concatStringsSep " " [
        "NIRI_SOCKET"
        "WAYLAND_DISPLAY"
        "XDG_CURRENT_DESKTOP"
        "XDG_SESSION_TYPE"
      ];
    in
      pkgs.writeScript "niri-startup-hook"
      # bash
      ''
        ${dash-shebang}
        ${lib.getExe' pkgs.dbus "dbus-update-activation-environment"} --systemd ${variables}
        ${systemctl} --user import-environment ${variables}
        ${systemctl} --user stop niri-session.target
        ${systemctl} --user start niri-session.target
      '';
    change_wallpaper = wallpaper-script;
    toggle_activate_linux =
      pkgs.writeScript "toggle-activate-linux"
      # bash
      ''
        ${dash-shebang}
        test "$(${systemctl} --user is-active activate-linux.service)" = "active" \
          && ${systemctl} --user stop activate-linux.service \
          || ${systemctl} --user start activate-linux.service
      '';

    # this causes these values (those used by wpctl) to be excluded from the
    # validation check that makes sure there are no unsubstituted values.
    DEFAULT_AUDIO_SINK = null;
    DEFAULT_AUDIO_SOURCE = null;
  };

  home.packages = let
    niri-wrapped = pkgs.symlinkJoin {
      name = "niri-wrapped";
      paths = [pkgs.niri];
      nativeBuildInputs = [pkgs.makeWrapper];
      postBuild = ''
        wrapProgram $out/bin/niri \
          --prefix PATH : ${pkgs.xwayland-satellite}/bin
      '';
    };
  in [
    niri-wrapped
    pkgs.wl-clipboard
    pkgs.wl-mirror
  ];

  xdg.portal = {
    enable = true;
    extraPortals = with pkgs; [
      xdg-desktop-portal-gnome
      xdg-desktop-portal-gtk
    ];
    config.common = {
      default = ["gtk"];
      "org.freedesktop.impl.portal.ScreenCast" = ["gnome"];
      "org.freedesktop.impl.portal.Secret" = ["gnome-keyring"];
    };
  };

  programs.swaylock = {
    enable = true;
    settings = let
      transparent = "00000000";
      black = "000000";
      default = "ff006e";
      clear = "ff9ed9";
      caps-lock = "d6e800";
      ver = "7000fc";
      wrong = "ff1212";
      bs = "9c000e";
    in {
      bs-hl-color = bs;
      caps-lock-bs-hl-color = bs;
      caps-lock-key-hl-color = caps-lock;
      color = black;
      indicator-radius = 250;
      indicator-thickness = 20;
      inside-caps-lock-color = transparent;
      inside-clear-color = transparent;
      inside-color = transparent;
      inside-ver-color = transparent;
      inside-wrong-color = transparent;
      key-hl-color = black;
      line-uses-inside = true;
      ring-caps-lock-color = caps-lock;
      ring-clear-color = clear;
      ring-color = default;
      ring-ver-color = ver;
      ring-wrong-color = wrong;
      separator-color = transparent;
      text-caps-lock-color = caps-lock;
      text-clear-color = clear;
      text-color = default;
      text-ver-color = ver;
      text-wrong-color = wrong;
    };
  };

  services.swayidle = let
    swaylock = lib.getExe pkgs.swaylock;
  in {
    enable = true;
    events = {
      lock = swaylock;
      before-sleep = swaylock;
      after-resume = "${wallpaper-script}";
    };
    timeouts = [
      {
        timeout = 180;
        command = swaylock;
      }
      {
        timeout = 600;
        command = "systemctl suspend";
      }
    ];
  };

  systemd.user = let
    graphical-target = "graphical-session.target";
  in {
    targets.niri-session = {
      Unit = {
        Description = "niri compositor session";
        Documentation = ["man:systemd.special(7)"];
        BindsTo = [graphical-target];
        Wants = [
          "graphical-session-pre.target"
          "xdg-desktop-autostart.target"
        ];
        After = ["graphical-session-pre.target"];
        Before = ["xdg-desktop-autostart.target"];
      };
    };
    services = {
      gammastep = {
        Unit = {
          Description = "Blue light reduction service";
          Wants = [graphical-target];
          After = [graphical-target];
        };

        Service = {
          Type = "simple";
          ExecStart =
            (lib.getExe' pkgs.gammastep "gammastep-indicator")
            + " -c "
            + (pkgs.writeText "gammastep.conf"
              # ini
              ''
                [general]
                temp-day=4600
                temp-night=3600
                dawn-time=7:00-8:00
                dusk-time=17:30-18:30
                brightness-day=1.0
                brightness-night=0.90
              '');
        };

        Install.WantedBy = [graphical-target];
      };
      activate-linux = {
        Unit = {
          Description = "Activate Linux";
          Wants = [graphical-target];
          After = [graphical-target];
        };

        Service = {
          Type = "simple";
          ExecStart = lib.getExe pkgs.activate-linux + " -s 0.8";
        };

        Install.WantedBy = [graphical-target];
      };
      swww-daemon = {
        Unit = {
          Description = "swww wallpaper daemon";
          Wants = [graphical-target];
          After = [graphical-target];
        };

        Service = {
          Type = "simple";
          ExecStart = lib.getExe' pkgs.swww "swww-daemon";
          Restart = "on-failure";
          RestartSec = 1;
          TimeoutSec = "30s";
        };

        Install.WantedBy = [graphical-target];
      };
      swww = {
        Unit = {
          Description = "cycle swww wallpaper";
          Wants = [graphical-target];
          After = [graphical-target "swww-daemon.service"];
        };

        Service = {
          Type = "oneshot";
          ExecStart = wallpaper-script;
        };

        Install.WantedBy = [graphical-target];
      };
    };
    timers = {
      swww = {
        Unit = {
          Description = "cycle swww wallpaper";
          Wants = [graphical-target];
          After = [graphical-target "swww-daemon.service"];
        };

        Timer = {
          OnUnitActiveSec = "5min";
          RandomizedDelaySec = "30s";
          AccuracySec = "30s";
        };

        Install.WantedBy = [graphical-target];
      };
    };
  };
}
