{ config, pkgs, lib, inputs, ... }:

{
  dconf.enable = true;
  gtk = {
    enable = true;
    theme = {
      package = (pkgs.catppuccin-gtk.override {
        accents = [ "mauve" ];
        variant = "mocha";
      });
      name = "Catppuccin-Mocha-Standard-Mauve-Dark";
    };
    font = {
      package = pkgs.raleway;
      name = "Raleway";
      size = 10;
    };
    iconTheme = {
      package = (pkgs.catppuccin-papirus-folders.override {
        flavor = "mocha";
        accent = "mauve";
      });
      name = "Papirus-Dark";
    };
    cursorTheme = {
      package = pkgs.catppuccin-cursors;
      name = "Catppuccin-Mocha-Mauve-Cursors";
      size = 48;
    };
    gtk2.configLocation = "${config.xdg.configHome}/gtk-2.0/gtkrc";
  };
  
  # home.pointerCursor = {
  #   name = "Catppuccin-Mocha-Mauve-Cursors";
  #   package = pkgs.catppuccin-cursors;
  #   size = 48;
  #   x11 = {
  #     enable = true;
  #     defaultCursor = "Catppuccin-Mocha-Mauve-Cursors";
  #   };
  # };
}
