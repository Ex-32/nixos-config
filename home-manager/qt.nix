{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  home.packages = with pkgs; [
    libsForQt5.qtstyleplugin-kvantum
    qt6Packages.qtstyleplugin-kvantum
  ];

  qt = {
    enable = true;
    platformTheme.name = "qtct";
    style = {
      name = "kvantum";
      # package = pkgs.libsForQt5.qtstyleplugin-kvantum;
    };
  };

  xdg.configFile = let
    theme = pkgs.catppuccin-kvantum.override {
      variant = "Mocha";
      accent = "Mauve";
    };
  in {
    "Kvantum/kvantum.kvconfig".text = ''
      [General]
      theme=Catppuccin-Mocha-Mauve
    '';

    "Kvantum/Catppuccin-Mocha-Mauve".source = "${theme}/share/Kvantum/Catppuccin-Mocha-Mauve";
  };
}
