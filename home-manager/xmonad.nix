{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  imports = [
    ./gtk.nix
    ./qt.nix
    ./kitty.nix
    ./picom.nix
  ];

  home.packages = [pkgs.xclip];

  xsession = {
    enable = true;
    initExtra =
      /*
      sh
      */
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
        wpctl = "${pkgs.wireplumber}/bin/wpctl";
        playerctl = "${pkgs.playerctl}/bin/playerctl";
        xclip = "${pkgs.xclip}/bin/xclip";
        maim = "${pkgs.maim}/bin/maim";
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
          xhost-hack = "${pkgs.xorg.xhost}/bin/xhost +SI:localuser:root";
        };
    };
  };

  systemd.user.services.taffybar = let
    taffybar-configured = pkgs.haskellPackages.callCabal2nix "taffybar-configured" ../config/taffybar {};
  in {
    Unit = {
      Description = "taffybar system bar";
      Wants = ["graphical-session.target"];
      After = ["graphical-session.target"];
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

  home.file = {
    ".config/rofi/config.rasi".source = ../config/rofi/config.rasi;
  };
}
