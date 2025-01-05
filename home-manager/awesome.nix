{
  config,
  osConfig,
  pkgs,
  lib,
  inputs,
  ...
}: let
  wallpaper-awesome = let
    core = "${pkgs.coreutils}/bin";
    home = config.home.homeDirectory;
    hostname = osConfig.networking.hostName;
  in
    pkgs.writeScript "wallpaper-script-awesome"
    # bash
    ''
      #!/bin/sh
      set -e

      # FIXME: don't hardcode wallpaper path
      WALLPAPER_PATH="${home}/documents/pictures/wallpapers/${hostname}"
      if [ ! -d "$WALLPAPER_PATH" ] ; then
        echo "wallpaper directory '$WALLPAPER_PATH' doesn't exist"
        exit 1
      fi
      RAND_PAPER="$WALLPAPER_PATH/$(${core}/ls -1 "$WALLPAPER_PATH" |\
        ${core}/shuf --random-source=/dev/urandom -n 1)"
      ${pkgs.dbus}/bin/dbus-send \
        --dest=org.awesomewm.awful \
        --type=method_call \
        --print-reply \
        / org.awesomewm.awful.Remote.Eval \
        "string:require('gears').wallpaper.maximized('$RAND_PAPER')"
    '';
in {
  imports = [
    ./gtk.nix
    ./kitty.nix
    ./picom.nix
    ./qt.nix
    ./systray.nix
  ];

  home.packages = with pkgs; [
    xclip
  ];

  xsession = {
    enable = true;
    initExtra =
      # bash
      ''
        export SHLVL=0
      '';
    profilePath = ".config/xprofile";
    scriptPath = ".config/xsession";
    windowManager.awesome = {
      enable = true;
    };
  };

  home.file = {
    ".config/awesome/rc.lua".source = let
      maim = "${pkgs.maim}/bin/maim";
      xclip-png = "${pkgs.xclip}/bin/xclip -selection clipboard -target image/png -i";
      xorg = pkgs.xorg;
    in
      pkgs.replaceVars ../config/awesome/rc.lua rec {
        # @variables@ to substitute
        ## variables
        home = config.home.homeDirectory;
        ## packages
        # brightnessctl = "${pkgs.brightnessctl}/bin/brightnessctl";
        kitty = "${pkgs.kitty}/bin/kitty";
        playerctl = "${pkgs.playerctl}/bin/playerctl";
        wpctl = "${pkgs.wireplumber}/bin/wpctl";
        tdrop = "${pkgs.tdrop}/bin/tdrop";
        # xdotool = "${pkgs.xdotool}/bin/xdotool";
        # xhost = "${xorg.xhost}/bin/xhost";
        # xsetroot = "${xorg.xsetroot}/bin/xsetroot";
        # ## scripts
        change_wallpaper = wallpaper-awesome;
        screenshot_full = "${maim} | ${xclip-png}";
        screenshot_select = "${maim} -s | ${xclip-png}";
        vol_down =
          pkgs.writeScript "vol-down"
          # python
          ''
            #!${pkgs.python3}/bin/python3

            import subprocess

            output = subprocess.run(
              ["${wpctl}", "get-volume", "@DEFAULT_AUDIO_SINK@"],
              capture_output=True,
            ).stdout

            # between '0' and '9' ascii
            vol = int(bytes([x for x in output if x >= 48 and x <= 57]))
            new_vol = (((vol - 1) // 5) * 5) / 100

            subprocess.run(
              ["${wpctl}", "set-volume", "-l", "1.5", "@DEFAULT_AUDIO_SINK@", str(new_vol)]
            )
          '';
        vol_up =
          pkgs.writeScript "vol-up"
          # python
          ''
            #!${pkgs.python3}/bin/python3

            import subprocess

            output = subprocess.run(
              ["${wpctl}", "get-volume", "@DEFAULT_AUDIO_SINK@"],
              capture_output=True,
            ).stdout

            # between '0' and '9' ascii
            vol = int(bytes([x for x in output if x >= 48 and x <= 57]))
            new_vol = (((vol + 5) // 5) * 5) / 100

            subprocess.run(
              ["${wpctl}", "set-volume", "-l", "1.5", "@DEFAULT_AUDIO_SINK@", str(new_vol)]
            )
          '';
      };
    ".config/awesome/theme".source = ../config/awesome/theme;
  };

  systemd.user = {
    services = {
      wallpaper = {
        Unit = {
          Description = "awesomeWM wallpaper service";
          Wants = ["graphical-session.target"];
          After = ["graphical-session.target"];
        };

        Service = {
          Type = "oneshot";
          ExecStart = wallpaper-awesome;
        };

        Install = {
          WantedBy = ["graphical-session.target"];
        };
      };
    };
    timers = {
      wallpaper = {
        Unit = {
          Description = "awesomeWM wallpaper timer";
          Wants = ["graphical-session.target"];
          After = ["graphical-session.target"];
        };

        Timer = {
          OnUnitActiveSec = "5min";
          RandomizedDelaySec = "30s";
          AccuracySec = "30s";
        };

        Install = {
          WantedBy = ["graphical-session.target"];
        };
      };
    };
  };
}
