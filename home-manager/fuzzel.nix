{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  home = {
    packages = [pkgs.fuzzel];
    file.".config/fuzzel/fuzzel.ini".text =
      # ini
      ''
        [main]
        font=Source Sans:size=8
        anchor=top
        lines=30
        line-height=14

        [colors]
        background=100c00ff
        text=fcfcfcff
        match=01d2fdff
        selection=ff006eff
        selection-text=100c00ff
        selection-match=fcfcfcff
        border=ff006eff

        [border]
        width=4
        radius=0
      '';
  };
}
