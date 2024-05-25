{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  imports = [
    ./gtk.nix
    ./kitty.nix
    ./picom.nix
    ./qt.nix
    ./systray.nix
  ];

  home.packages = [pkgs.xclip];

  xsession = {
    enable = true;
    initExtra =
      # bash
      ''
        export SHLVL=0
      '';
    profilePath = ".config/xprofile";
    scriptPath = ".config/xsession";
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      extraPackages = haskellPackages:
        with haskellPackages; [
          taffybar
        ];
      config = let
        maim = "${pkgs.maim}/bin/maim";
        playerctl = "${pkgs.playerctl}/bin/playerctl";
        wpctl = "${pkgs.wireplumber}/bin/wpctl";
        xclip = "${pkgs.xclip}/bin/xclip";
        xorg = pkgs.xorg;
      in
        pkgs.substituteAll rec {
          src = ../config/xmonad/xmonad.hs;

          # @variables@ to substitute
          rofi = lib.strings.concatStringsSep " " [
            "${pkgs.rofi}/bin/rofi"
            "-show drun"
            "-modi drun"
            "-scroll-method 0"
            "-drun-match-fields all"
            "-drun-display-format {name}"
            "-no-drun-show-actions"
            "-terminal '${kitty}'"
            "-theme config"
          ];
          kitty = "${pkgs.kitty}/bin/kitty";
          media_next = "${playerctl} next";
          media_play = "${playerctl} play-pause";
          media_prev = "${playerctl} previous";
          screenshot_full = "sh -c '${maim} | ${xclip} -selection clipboard -target image/png -i'";
          screenshot_select = "sh -c '${maim} -s | ${xclip} -selection clipboard -target image/png -i'";
          vol_down = "${wpctl} set-volume @DEFAULT_AUDIO_SINK@ 5%-";
          vol_mute = "${wpctl} set-mute @DEFAULT_AUDIO_SINK@ toggle";
          vol_up = "${wpctl} set-volume -l 1.5 @DEFAULT_AUDIO_SINK@ 5%+";
          xhost_hack = "${xorg.xhost}/bin/xhost +SI:localuser:root";
          xsetroot_hack = "${xorg.xsetroot}/bin/xsetroot -cursor_name left_ptr";
        };
    };
  };

  home.file = {
    ".config/rofi/config.rasi".source = ../config/rofi/config.rasi;
    ".xmonad/bin/wallpaper.sh" = {
      executable = true;
      text = let
        core = "${pkgs.coreutils}/bin";
      in
        # bash
        ''
          #!/bin/sh
          set -e
          # FIXME: don't hardcode wallpaper path
          WALLPAPER_PATH="${config.home.homeDirectory}/documents/pictures/wallpapers/current"
          if [ ! -d "$WALLPAPER_PATH" ] ; then
            echo "wallpaper directory '$WALLPAPER_PATH' doesn't exist"
            exit 1
          fi
          RAND_PAPER="$WALLPAPER_PATH/$(${core}/ls -1 "$WALLPAPER_PATH" |\
            ${core}/shuf --random-source=/dev/urandom -n 1)"
          ${pkgs.feh}/bin/feh --no-fehbg --bg-fill $RAND_PAPER
        '';
    };
  };

  systemd.user = {
    services = {
      taffybar = let
        taffybar-configured = pkgs.haskellPackages.callCabal2nix "taffybar-configured" ../config/taffybar {};
      in {
        Unit = {
          Description = "taffybar system bar service";
          Wants = [
            "graphical-session.target"
            "status-notifier-watcher.service"
          ];
          After = [
            "graphical-session.target"
            "status-notifier-watcher.service"
          ];
        };

        Service = {
          Type = "simple";
          ExecStart = "${taffybar-configured}/bin/taffybar";
          Restart = "on-failure";
          RestartSec = 1;
          TimeoutSec = "30s";
        };

        Install = {
          WantedBy = ["graphical-session.target"];
        };
      };
      status-notifier-watcher = let
        sni = pkgs.haskellPackages.status-notifier-item;
      in {
        Unit = {
          Description = "StatusNotifierWatcher implementation";
          Wants = ["graphical-session.target"];
          After = ["graphical-session.target"];
        };

        Service = {
          Type = "simple";
          ExecStart = "${sni}/bin/status-notifier-watcher";
          Restart = "on-failure";
          RestartSec = 1;
          TimeoutSec = "30s";
        };

        Install = {
          WantedBy = ["graphical-session.target"];
        };
      };
      fehbg = {
        Unit = {
          Description = "feh wallpaper service";
          Wants = ["graphical-session.target"];
          After = ["graphical-session.target"];
        };

        Service = {
          Type = "oneshot";
          ExecStart = "${config.home.homeDirectory}/.xmonad/bin/wallpaper.sh";
        };

        Install = {
          WantedBy = ["graphical-session.target"];
        };
      };
    };
    timers = {
      fehbg = {
        Unit = {
          Description = "feh wallpaper timer";
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
