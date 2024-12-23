{
  config,
  osConfig,
  pkgs,
  lib,
  inputs,
  ...
}: let
  xmonad-wallpaper = let
    core = "${pkgs.coreutils}/bin";
    home = config.home.homeDirectory;
    hostname = osConfig.networking.hostName;
  in
    pkgs.writeScript "xmonad-wallpaper"
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
      ${pkgs.feh}/bin/feh --no-fehbg --bg-fill $RAND_PAPER
    '';
in {
  imports = [
    ./gtk.nix
    ./kitty.nix
    ./picom.nix
    ./polybar.nix
    ./qt.nix
    ./systray.nix
  ];

  home.packages = with pkgs; [
    xclip
    xmonadctl
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
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      haskellPackages = pkgs.haskellPackages.extend (self: super: {
        xmonad = super.xmonad_0_18_0;
      });
      config = let
        maim = "${pkgs.maim}/bin/maim";
        xclip-png = "${pkgs.xclip}/bin/xclip -selection clipboard -target image/png -i";
        xorg = pkgs.xorg;
      in
        pkgs.replaceVars ../config/xmonad/xmonad.hs rec {
          # @variables@ to substitute
          ## packages
          brightnessctl = "${pkgs.brightnessctl}/bin/brightnessctl";
          kitty = "${pkgs.kitty}/bin/kitty";
          playerctl = "${pkgs.playerctl}/bin/playerctl";
          wpctl = "${pkgs.wireplumber}/bin/wpctl";
          xhost = "${xorg.xhost}/bin/xhost";
          rlaunch = "${pkgs.rlaunch}/bin/rlaunch";
          xsetroot = "${xorg.xsetroot}/bin/xsetroot";
          change_wallpaper = xmonad-wallpaper;
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
    };
  };

  home.file = {
    ".config/rofi/config.rasi".source = ../config/rofi/config.rasi;
  };

  systemd.user = {
    services = {
      # taffybar = let
      #   taffybar-configured = pkgs.haskellPackages.callCabal2nix "taffybar-configured" ../config/taffybar {};
      # in {
      #   Unit = {
      #     Description = "taffybar system bar service";
      #     Wants = [
      #       "graphical-session.target"
      #       "status-notifier-watcher.service"
      #     ];
      #     After = [
      #       "graphical-session.target"
      #       "status-notifier-watcher.service"
      #     ];
      #   };
      #
      #   Service = {
      #     Type = "simple";
      #     ExecStart = "${taffybar-configured}/bin/taffybar";
      #     Restart = "on-failure";
      #     RestartSec = 1;
      #     TimeoutSec = "30s";
      #   };
      #
      #   Install = {
      #     WantedBy = ["graphical-session.target"];
      #   };
      # };
      # status-notifier-watcher = let
      #   sni = pkgs.haskellPackages.status-notifier-item;
      # in {
      #   Unit = {
      #     Description = "StatusNotifierWatcher implementation";
      #     Wants = ["graphical-session.target"];
      #     After = ["graphical-session.target"];
      #   };
      #
      #   Service = {
      #     Type = "simple";
      #     ExecStart = "${sni}/bin/status-notifier-watcher";
      #     Restart = "on-failure";
      #     RestartSec = 1;
      #     TimeoutSec = "30s";
      #   };
      #
      #   Install = {
      #     WantedBy = ["graphical-session.target"];
      #   };
      # };
      fehbg = {
        Unit = {
          Description = "feh wallpaper service";
          Wants = ["graphical-session.target"];
          After = ["graphical-session.target"];
        };

        Service = {
          Type = "oneshot";
          ExecStart = xmonad-wallpaper;
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
