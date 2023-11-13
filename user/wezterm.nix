{ config, pkgs, lib, inputs, ... }:

{
  imports = [
    ./nerdfont.nix
  ];

  programs.wezterm = {
    enable = true;
    extraConfig = ''
      local config = {}
      if wezterm.config_builder then
        config = wezterm.config_builder()
      end

      config.color_scheme = "Catppuccin Mocha"
      config.font = wezterm.font "FiraCode Nerd Font"
      config.font_size = 14
      config.default_cursor_style = "SteadyBar"
      config.hide_mouse_cursor_when_typing = false
      config.window_background_opacity = 0.7
      config.window_decorations = "NONE"
      config.window_padding = {
        left = 0,
        right = 0,
        top = 0,
        bottom = 0,
      }
      config.use_fancy_tab_bar = false
      config.hide_tab_bar_if_only_one_tab = true

      return config
    '';
  };
}
