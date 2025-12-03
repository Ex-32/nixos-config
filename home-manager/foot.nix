{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  programs.foot = {
    enable = true;
    server.enable = false;
    settings = {
      main = {
        font = "FiraCode Nerd Font:size=11";
      };
      colors = {
        foreground = "abf9fd";
        background = "100c00";
        selection-foreground = "000000";
        selection-background = "ff006e";

        #: Cursor colors

        cursor = "ff006e 100c00";

        #: URL underline color when hovering with mouse

        # url_color                       #ff006e

        #: kitty window border colors and terminal bell colors

        # active_border_color             #00ffff
        # inactive_border_color           #161021
        # visual_bell_color               none
        # bell_border_color               #ff5a00

        #: Tab bar colors

        # active_tab_foreground           #100c00
        # active_tab_background           #ff006e
        # inactive_tab_foreground         #4a434a
        # inactive_tab_background         #9a8898
        # tab_bar_background              none
        # tab_bar_margin_color            none

        #: Colors for marks (marked text in the terminal)

        # mark1_foreground black
        # mark1_background #fe552c
        # mark2_foreground black
        # mark2_background #afb48b
        # mark3_foreground black
        # mark3_background #ff85ac

        #: The basic 16 colors

        #: black
        regular0 = "252726";
        bright0 = "848484";

        #: red
        regular1 = "ff006e";
        bright1 = "ff1212";

        #: green
        regular2 = "00b300";
        bright2 = "00d500";

        #: yellow
        regular3 = "ace700";
        bright3 = "d6e800";

        #: blue
        regular4 = "4105fb";
        bright4 = "7000fc";

        #: magenta
        regular5 = "9c00fc";
        bright5 = "cd00f0";

        #: cyan
        regular6 = "00b0df";
        bright6 = "25d0fe";

        #: white
        regular7 = "9a8898";
        bright7 = "bdadb8";
      };
    };
  };
}
