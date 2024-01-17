{ config, pkgs, lib, inputs, ... }:

{
  imports = [
    ./systray.nix
    ./nerdfont.nix
  ];

  programs.tint2 = {
    enable = true;
    extraConfig = ''
      #-------------------------------------
      # Backgrounds
      # Background 1: Panel
      rounded = 5
      border_width = 1
      border_sides = TLR
      background_color = #000000 80
      border_color = #555555 80
      background_color_hover = #000000 80
      border_color_hover = #555555 80
      background_color_pressed = #000000 80
      border_color_pressed = #555555 80

      # Background 2: Default task, Iconified task
      rounded = 5
      border_width = 1
      border_sides = TBLR
      background_color = #777777 0
      border_color =FiraCode Nerd Font 8 #777777 0
      background_color_hover = #777777 4
      border_color_hover = #cccccc 30
      background_color_pressed = #333333 4
      border_color_pressed = #777777 30

      # Background 3: Active task
      rounded = 5
      border_width = 1
      border_sides = TBLR
      background_color = #555555 10
      border_color = #ffffff 60
      background_color_hover = #cccccc 10
      border_color_hover = #ffffff 60
      background_color_pressed = #555555 10
      border_color_pressed = #ffffff 60

      # Background 4: Urgent task
      rounded = 5
      border_width = 1
      border_sides = TBLR
      background_color = #aa4400 100
      border_color = #aa7733 100
      background_color_hover = #aa4400 100
      border_color_hover = #aa7733 100
      background_color_pressed = #aa4400 100
      border_color_pressed = #aa7733 100

      # Background 5: Tooltip
      rounded = 2
      border_width = 1
      border_sides = TBLR
      background_color = #ffffaa 100
      border_color = #999999 100
      background_color_hover = #ffffaa 100
      border_color_hover = #999999 100
      background_color_pressed = #ffffaa 100
      border_color_pressed = #999999 100

      # Background 6: Inactive desktop name
      rounded = 2
      border_width = 1
      border_sides = TBLR
      background_color = #777777 0
      border_color = #777777 30
      background_color_hover = #777777 4
      border_color_hover = #cccccc 30
      background_color_pressed = #777777 0
      border_color_pressed = #777777 30

      # Background 7: Active desktop name
      rounded = 2
      border_width = 1
      border_sides = TBLR
      background_color = #555555 10
      border_color = #ffffff 60
      background_color_hover = #555555 10
      border_color_hover = #ffffff 60
      background_color_pressed = #555555 10
      border_color_pressed = #ffffff 60

      #-------------------------------------
      # Panel
      panel_items = TBCS
      panel_size = 100% 32
      panel_margin = 0 0
      panel_padding = 4 2 4
      panel_background_id = 1
      wm_menu = 1
      panel_dock = 0
      panel_position = top center horizontal
      panel_layer = normal
      panel_monitor = all
      panel_shrink = 0
      autohide = 0
      autohide_show_timeout = 0
      autohide_hide_timeout = 0.5
      autohide_height = 2
      strut_policy = follow_size
      panel_window_name = tint2
      disable_transparency = 0
      mouse_effects = 1
      font_shadow = 0
      mouse_hover_icon_asb = 100 0 10
      mouse_pressed_icon_asb = 100 0 0

      #-------------------------------------
      # Taskbar
      taskbar_mode = single_desktop
      taskbar_hide_if_empty = 0
      taskbar_padding = 0 0 2
      taskbar_background_id = 0
      taskbar_active_background_id = 0
      taskbar_name = 1
      taskbar_hide_inactive_tasks = 0
      taskbar_hide_different_monitor = 0
      taskbar_hide_different_desktop = 0
      taskbar_always_show_all_desktop_tasks = 0
      taskbar_name_padding = 6 3
      taskbar_name_background_id = 6
      taskbar_name_active_background_id = 7
      taskbar_name_font = sans bold 9
      taskbar_name_font_color = #dddddd 100
      taskbar_name_active_font_color = #dddddd 100
      taskbar_distribute_size = 1
      taskbar_sort_order = none
      task_align = left

      #-------------------------------------
      # Task
      task_text = 1
      task_icon = 1
      task_centered = 1
      urgent_nb_of_blink = 100000
      task_maximum_size = 140 35
      task_padding = 4 3 4
      task_font = sans 8
      task_tooltip = 1
      task_font_color = #eeeeee 100
      task_icon_asb = 100 0 0
      task_background_id = 2
      task_active_background_id = 3
      task_urgent_background_id = 4
      task_iconified_background_id = 2
      mouse_left = toggle_iconify
      mouse_middle = none
      mouse_right = close
      mouse_scroll_up = prev_task
      mouse_scroll_down = next_task

      #-------------------------------------
      # System tray (notification area)
      systray_padding = 0 0 2
      systray_background_id = 0
      systray_sort = ascending
      systray_icon_size = 22
      systray_icon_asb = 100 0 0
      systray_monitor = 1
      systray_name_filter = 

      #-------------------------------------
      # Launcher
      launcher_padding = 0 0 2
      launcher_background_id = 0
      launcher_icon_background_id = 0
      launcher_icon_size = 22
      launcher_icon_asb = 100 0 0
      launcher_icon_theme_override = 0
      startup_notifications = 1
      launcher_tooltip = 1
      launcher_item_app = tint2conf.desktop
      launcher_item_app = firefox.desktop
      launcher_item_app = iceweasel.desktop
      launcher_item_app = chromium-browser.desktop
      launcher_item_app = google-chrome.desktop
      launcher_item_app = x-terminal-emulator.desktop

      #-------------------------------------
      # Clock
      time1_format = %H:%M
      time2_format = 
      time1_font = FiraCode Nerd Font 12
      time1_timezone = 
      time2_timezone = 
      time2_font = 
      clock_font_color = #eeeeee 100
      clock_padding = 1 0
      clock_background_id = 0
      clock_tooltip = 
      clock_tooltip_timezone = 
      clock_lclick_command = 
      clock_rclick_command = 
      clock_mclick_command = 
      clock_uwheel_command = 
      clock_dwheel_command = 

      #-------------------------------------
      # Battery
      battery_tooltip = 1
      battery_low_status = 10
      battery_low_cmd = xmessage 'tint2: Battery low!'
      battery_full_cmd = 
      bat1_font = sans 8
      bat2_font = sans 6
      battery_font_color = #eeeeee 100
      bat1_format = 
      bat2_format = 
      battery_padding = 1 0
      battery_background_id = 0
      battery_hide = 101
      battery_lclick_command = 
      battery_rclick_command = 
      battery_mclick_command = 
      battery_uwheel_command = 
      battery_dwheel_command = 
      ac_connected_cmd = 
      ac_disconnected_cmd = 

      #-------------------------------------
      # Tooltip
      tooltip_show_timeout = 0.5
      tooltip_hide_timeout = 0.1
      tooltip_padding = 2 2
      tooltip_background_id = 5
      tooltip_font_color = #222222 100
      tooltip_font = sans 9
    '';
  };
}
