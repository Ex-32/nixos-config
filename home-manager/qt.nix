{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  home.packages = [
    (pkgs.catppuccin-kvantum.override {
      variant = "Mocha";
      accent = "Mauve";
    })
  ];

  qt = {
    enable = true;
    platformTheme.name = "adwaita";
    style = {
      name = "kvantum-dark";
      package = pkgs.libsForQt5.qtstyleplugin-kvantum;
    };
  };
}
