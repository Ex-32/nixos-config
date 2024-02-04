{ config, pkgs, lib, inputs, ... } : let
  cursor-size = 48;
  cursor-name = "Catppuccin-Mocha-Mauve-Cursors";
in {
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
      package = pkgs.catppuccin-cursors.mochaMauve;
      name = cursor-name;
      size = cursor-size;
    };
    gtk2.configLocation = "${config.xdg.configHome}/gtk-2.0/gtkrc";
  };
  
  home.pointerCursor = {
    package = pkgs.catppuccin-cursors.mochaMauve;
    name = cursor-name;
    size = cursor-size;
    gtk.enable = true;
  };
  
  # HACK: disables the creation of the legacy ~/.icons directory by manually 
  # disabling the files created by the home-manager module, a better solution
  # would be a way to blacklist any key in home.file starting with ".icons"
  home.file.".icons/default/index.theme".enable = false;
  home.file.".icons/${config.home.pointerCursor.name}".enable = false;
}
