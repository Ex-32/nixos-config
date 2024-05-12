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
        ${pkgs.qtile}/bin/qtile start
      '';
    profilePath = ".config/xprofile";
    scriptPath = ".config/xsession";
  };

  home.file = {
    ".config/qtile/config.py".source = let
      wpctl = "${pkgs.wireplumber}/bin/wpctl";
      playerctl = "${pkgs.playerctl}/bin/playerctl";
    in
      pkgs.substituteAll rec {
        src = ../config/qtile/config.py;

        # @variables@ to substitute
        kitty = "${pkgs.kitty}/bin/kitty -1";
        dropterm = "${pkgs.tdrop}/bin/tdrop -h 82% -w 88% -x 6% -y 9% kitty --class floating";
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

        vol_mute = "${wpctl} set-mute @DEFAULT_AUDIO_SINK@ toggle";
        vol_down = "${wpctl} set-volume @DEFAULT_AUDIO_SINK@ 5%-";
        vol_up = "${wpctl} set-volume -l 1.5 @DEFAULT_AUDIO_SINK@ 5%+";

        media_prev = "${playerctl} previous";
        media_play = "${playerctl} play-pause";
        media_next = "${playerctl} next";

        xhost-hack = "${pkgs.xorg.xhost}/bin/xhost +SI:localuser:root";
      };

    ".config/rofi/config.rasi".source = ../config/rofi/config.rasi;
  };
}
