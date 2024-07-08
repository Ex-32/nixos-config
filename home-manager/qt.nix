{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  home.packages = [pkgs.libsForQt5.lightly];

  qt = {
    enable = true;
    platformTheme.name = "qtct";
  };

  xdg.configFile = let
    colors = pkgs.writeText "catppuccin-qtct-colors" ''
      [ColorScheme]
      active_colors=${builtins.concatStringsSep ", " [
        "#ffcdd6f4"
        "#ff1e1e2e"
        "#ffa6adc8"
        "#ff9399b2"
        "#ff45475a"
        "#ff6c7086"
        "#ffcdd6f4"
        "#ffcdd6f4"
        "#ffcdd6f4"
        "#ff1e1e2e"
        "#ff181825"
        "#ff7f849c"
        "#ffcba6f7"
        "#ff1e1e2e"
        "#ff89b4fa"
        "#fff38ba8"
        "#ff1e1e2e"
        "#ffcdd6f4"
        "#ff11111b"
        "#ffcdd6f4"
        "#807f849c"
      ]}
      disabled_colors=${builtins.concatStringsSep ", " [
        "#ffa6adc8"
        "#ff1e1e2e"
        "#ffa6adc8"
        "#ff9399b2"
        "#ff45475a"
        "#ff6c7086"
        "#ffa6adc8"
        "#ffa6adc8"
        "#ffa6adc8"
        "#ff1e1e2e"
        "#ff11111b"
        "#ff7f849c"
        "#ffcba6f7"
        "#ff45475a"
        "#ff89b4fa"
        "#fff38ba8"
        "#ff1e1e2e"
        "#ffcdd6f4"
        "#ff11111b"
        "#ffcdd6f4"
        "#807f849c"
      ]}
      inactive_colors=${builtins.concatStringsSep ", " [
        "#ffcdd6f4"
        "#ff1e1e2e"
        "#ffa6adc8"
        "#ff9399b2"
        "#ff45475a"
        "#ff6c7086"
        "#ffcdd6f4"
        "#ffcdd6f4"
        "#ffcdd6f4"
        "#ff1e1e2e"
        "#ff181825"
        "#ff7f849c"
        "#ffcba6f7"
        "#ffa6adc8"
        "#ff89b4fa"
        "#fff38ba8"
        "#ff1e1e2e"
        "#ffcdd6f4"
        "#ff11111b"
        "#ffcdd6f4"
        "#807f849c"
      ]}
    '';
    config = pkgs.writeText "qtct-config" ''
      [Appearance]
      color_scheme_path=${colors}
      custom_palette=true
      icon_theme=Papirus-Dark
      standard_dialogs=default
      style=Lightly

      [Fonts]
      fixed="FiraCode Nerd Font,14,-1,5,50,0,0,0,0,0,Regular"
      general="Source Sans 3,16,-1,5,50,0,0,0,0,0,Regular"

      [Interface]
      activate_item_on_single_click=1
      buttonbox_layout=0
      cursor_flash_time=1000
      dialog_buttons_have_icons=1
      double_click_interval=400
      gui_effects=@Invalid()
      keyboard_scheme=2
      menus_have_icons=true
      show_shortcuts_in_context_menus=true
      stylesheets=@Invalid()
      toolbutton_style=4
      underline_shortcut=1
      wheel_scroll_lines=3

      [SettingsWindow]
      geometry=@ByteArray(\x1\xd9\xd0\xcb\0\x3\0\0\0\0\0\0\0\0\0\0\0\0\x5W\0\0\x5\x99\0\0\0\0\0\0\0\0\0\0\b\xcf\0\0\x5\xb9\0\0\0\0\x2\0\0\0\b\xd0\0\0\0\0\0\0\0\0\0\0\x5W\0\0\x5\x99)

      [Troubleshooting]
      force_raster_widgets=1
      ignored_applications=@Invalid()
    '';
  in {
    "qt5ct/qt5ct.conf".source = config;
    "qt6ct/qt6ct.conf".source = config;
  };
}
