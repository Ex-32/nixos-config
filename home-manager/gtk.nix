{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  dconf = {
    enable = true;
    settings."org/gnome/desktop/interface".color-scheme = "prefer-dark";
  };

  gtk = {
    enable = true;
    theme = {
      package = pkgs.arc-theme;
      name = "Arc-Dark";
      # package = pkgs.gnome-themes-extra;
      # name = "Adwaita-dark";
    };
    font = {
      package = pkgs.source-sans;
      name = "Source Sans 3";
      size = 14;
    };
    iconTheme = {
      package = pkgs.papirus-icon-theme;
      name = "Papirus-Dark";
    };
    # cursorTheme = {
    #   package = pkgs.catppuccin-cursors.mochaMauve;
    #   name = cursor-name;
    #   size = cursor-size;
    # };
    gtk2.configLocation = "${config.xdg.configHome}/gtk-2.0/gtkrc";
  };

  # home.packages = with pkgs; [
  #   hicolor-icon-theme
  #   gnome-themes-extra
  # ];

  # home.pointerCursor = {
  #   package = pkgs.catppuccin-cursors.mochaMauve;
  #   name = cursor-name;
  #   size = cursor-size
  #   gtk.enable = true;
  # };

  # HACK: disables the creation of the legacy ~/.icons directory by manually
  # disabling the files created by the home-manager module, a better solution
  # would be a way to blacklist any key in home.file starting with ".icons"
  home.file.".icons/default/index.theme".enable = false;
  # home.file.".icons/${config.home.pointerCursor.name}".enable = false;
}
