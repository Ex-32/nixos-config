{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: let
  cursor-size = 48;
  cursor-name = "Catppuccin-Mocha-Mauve-Cursors";
in {
  dconf = {
    enable = true;
    settings."org/gnome/desktop/interface".color-scheme = "prefer-dark";
  };

  gtk = {
    enable = true;
    theme = {
      name = "Adwaita-dark";
      package = pkgs.gnome-themes-extra;
    };
    font = {
      package = pkgs.source-sans;
      name = "Source Sans 3";
      size = 14;
    };
    iconTheme = {
      package = let
        version = "30.0";
      in
        pkgs.stdenvNoCC.mkDerivation {
          pname = "suru-plus";
          inherit version;

          src = pkgs.fetchFromGitHub {
            owner = "gusbemacbe";
            repo = "suru-plus";
            rev = "v${version}";
            hash = "sha256-YrjIrqauqSXnP1FylynC+nWIJfMZvDj/WH9NgochbKI=";
          };

          nativeBuildInputs = [pkgs.gtk3];
          propagatedBuildInputs = with pkgs; [
            breeze-icons
            hicolor-icon-theme
          ];
          dontDropIconThemeCache = true;

          installPhase = ''
            runHook preInstall

            mkdir -p $out/share/icons
            mv "./Suru++" "$out/share/icons"
            gtk-update-icon-cache --force "$out/share/icons/Suru++"

            runHook postInstall
          '';
        };
      name = "Suru++ 25";
    };
    # cursorTheme = {
    #   package = pkgs.catppuccin-cursors.mochaMauve;
    #   name = cursor-name;
    #   size = cursor-size;
    # };
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
