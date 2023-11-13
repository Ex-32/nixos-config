{ config, pkgs, lib, inputs, ... }:

{
  programs.tmux = {
    enable = true;
    clock24 = true;
    mouse = true;
    sensibleOnTop = true;
    keyMode = "vi";
    shortcut = "Space";
    baseIndex = 1;
    plugins = with pkgs.tmuxPlugins; [
      catppuccin
      vim-tmux-navigator
      yank
    ];
    extraConfig = ''
      # use 24-bit color if supported
      set-option -sa terminal-overrides ",xterm*:Tc"

      # carry over current directory when creating new pane
      bind '"' split-window -v -c "#{pane_current_path}"
      bind % split-window -h -c "#{pane_current_path}"
    '';
  };
}
