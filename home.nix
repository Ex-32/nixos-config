{ config, pkgs, lib, inputs, ... }: let
in {

  xdg.userDirs = let
    home = config.home.homeDirectory;
  in {
    enable = true;
    createDirectories = true;
    desktop = null;
    documents = "${home}/documents";
    download = "${home}/downloads";
    music = "${home}/documents/music";
    pictures = "${home}/documents/pictures";
    publicShare = null;
    templates = null;
    videos = "${home}/documents/videos";
  };

  home.packages = with pkgs; [
    _1password-gui
    android-studio
    android-tools
    arduino
    aspell # for emacs spellchecking
    aspellDicts.en
    aspellDicts.en-computers
    aspellDicts.en-science
    brightnessctl
    (catppuccin-kvantum.override {
      variant = "Mocha";
      accent  = "Mauve";
    })
    comma
    discord
    firefox-devedition
    gh
    (nerdfonts.override {
      fonts = [
        "FiraCode"
        "NerdFontsSymbolsOnly"
      ];
    })
    godot_4
    gparted
    grim
    lazygit
    home-manager
    ncspot
    neovim
    nixd
    nodejs
    obs-studio
    onlyoffice-bin
    playerctl
    rclone
    rofi-wayland
    signal-desktop
    slurp
    starship
    swayidle
    system-config-printer
    texlive.combined.scheme-full
    xdg-utils
    wl-clipboard
    wl-mirror
    xdg-desktop-portal-wlr
    xorg.xhost
    zathura
  ];

  wayland.windowManager.hyprland = {
    enable = false;
    systemd.enable = true;
    settings = {
      "$mod" = "SUPER";
      general = {
        gaps_in = 3;
        gaps_out = 6;
        border_size = 1;
        "col.active_border" = "rgba(cba6f7ff) rgba(74c7ecff) 45deg";
        "col.inactive_border" = "rgba(313244ff)";
      };
      input = {
        kb_layout = "us";
        kb_options = "compose:ralt";
        natural_scroll = true;
        touchpad = {
          natural_scroll = true;
          tap-to-click = false;
          disable_while_typing = false;
          clickfinger_behavior = true;
        };
        sensitivity = 0.6;
      };
      decoration = {
        blur = {
          enabled = false;
        };
        drop_shadow = false;
      };
      animations = {
        enabled = true;
        bezier = [
          "myBezier, 0.05, 0.9, 0.1, 1.05"
        ];
        animation = [
          "windows, 1, 4, myBezier"
          "windowsOut, 1, 4, default, popin 80%"
          "border, 1, 4, default"
          "borderangle, 1, 2, default"
          "fade, 1, 4, default"
          "workspaces, 1, 4, default"
          "specialWorkspace, 1, 2, default, slidevert"
        ];
      };
      gestures = {
        workspace_swipe = true;
        workspace_swipe_forever = true;
        workspace_swipe_direction_lock = false;
        # workspace_swipe_numbered = true;
        workspace_swipe_distance = 250;
        workspace_swipe_min_speed_to_force = 10;
      };
      misc = {
        force_hypr_chan = true;
      };
      dwindle = {
        special_scale_factor = 0.9;
      };
      master = {
        special_scale_factor = 0.9;
      };
      monitor = [
        ", preferred, auto, 1"
      ];
      # normal keybinds
      bind = [
        "$mod, 1, workspace, 1"
        "$mod, 2, workspace, 2"
        "$mod, 3, workspace, 3"
        "$mod, 4, workspace, 4"
        "$mod, 5, workspace, 5"
        "$mod, 6, workspace, 6"
        "$mod, 7, workspace, 7"
        "$mod, 8, workspace, 8"
        "$mod, 9, workspace, 9"

        "$mod SHIFT, 1, movetoworkspace, 1"
        "$mod SHIFT, 2, movetoworkspace, 2"
        "$mod SHIFT, 3, movetoworkspace, 3"
        "$mod SHIFT, 4, movetoworkspace, 4"
        "$mod SHIFT, 5, movetoworkspace, 5"
        "$mod SHIFT, 6, movetoworkspace, 6"
        "$mod SHIFT, 7, movetoworkspace, 7"
        "$mod SHIFT, 8, movetoworkspace, 8"
        "$mod SHIFT, 9, movetoworkspace, 9"

        "$mod, q, exec, wezterm"
        "$mod, d, exec, sh -c 'pkill rofi || exec ~/.config/rofi/bin/launcher'"
        "$mod, Semicolon, exec, swaylock"
        "$mod, Print, exec, sh -c 'grim - | wl-copy'"
        "$mod SHIFT, Print, exec, sh -c 'slurp | grim -g - - | wl-copy'"

        ", XF86AudioPrev, exec, playerctl previous"
        ", XF86AudioPlay, exec, playerctl play-pause"
        ", XF86AudioNext, exec, playerctl next"

        "$mod, c, killactive"
        "$mod, Tab, togglespecialworkspace"
        "$mod, f, togglefloating"
        "$mod SHIFT, f, fullscreen"

        "$mod, left, movefocus, l"
        "$mod, right, movefocus, r"
        "$mod, up, movefocus, u"
        "$mod, down, movefocus, d"

        "$mod, h, movefocus, l"
        "$mod, j, movefocus, d"
        "$mod, k, movefocus, u"
        "$mod, l, movefocus, r"

        "$mod SHIFT, left, movewindow, l"
        "$mod SHIFT, right, movewindow, r"
        "$mod SHIFT, up, movewindow, u"
        "$mod SHIFT, down, movewindow, d"

        "$mod SHIFT, h, movewindow, l"
        "$mod SHIFT, j, movewindow, d"
        "$mod SHIFT, k, movewindow, u"
        "$mod SHIFT, l, movewindow, r"
      ];
      # keybinds that'll repeat if held
      binde = [
        ", XF86AudioLowerVolume, exec, wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-"
        ", XF86AudioRaiseVolume, exec, wpctl set-volume -l 1.5 @DEFAULT_AUDIO_SINK@ 5%+"
        ", XF86MonBrightnessDown, exec, brightnessctl set 5%-"
        ", XF86MonBrightnessUp, exec, brightnessctl set 5%+"
      ];
      # keybinds that can be invoked even while locked
      bindl = [
        ", XF86AudioMute, exec, wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"
      ];
      # mouse bindings
      bindm = [
        "$mod, mouse:272, movewindow"
        "$mod, mouse:273, resizewindow"
      ];
      exec = [
        "xhost +SI:localuser:root"
      ];
      exec-once = [
        "[workspace special] wezterm start --always-new-process sh -c 'while :; do $SHELL; hyprctl dispatch togglespecialworkspace; clear; done'"
      ];
    };
  };

  wayland.windowManager.sway = {
    enable = true;
    extraConfigEarly = ''
      set $rosewater #f5e0dc
      set $flamingo  #f2cdcd
      set $pink      #f5c2e7
      set $mauve     #cba6f7
      set $red       #f38ba8
      set $maroon    #eba0ac
      set $peach     #fab387
      set $green     #a6e3a1
      set $teal      #94e2d5
      set $sky       #89dceb
      set $sapphire  #74c7ec
      set $blue      #89b4fa
      set $lavender  #b4befe
      set $text      #cdd6f4
      set $subtext1  #bac2de
      set $subtext0  #a6adc8
      set $overlay2  #9399b2
      set $overlay1  #7f849c
      set $overlay0  #6c7086
      set $surface2  #585b70
      set $surface1  #45475a
      set $surface0  #313244
      set $base      #1e1e2e
      set $mantle    #181825
      set $crust     #11111b  
    ''; # catppuccin-mocha colors
    config = rec {
      modifier = "Mod4";
      terminal = "wezterm";
      gaps = {
        inner = 0;
        outer = 0;
      };
      colors = {
        focused = {
          border = "$mauve";
          background = "$base";
          text = "$text";
          indicator = "$mauve";
          childBorder = "$mauve";
        };
        focusedInactive = {
          border = "$overlay0";
          background = "$base";
          text = "$text";
          indicator = "$mauve";
          childBorder = "$overlay0";
        };
        unfocused = {
          border = "$overlay0";
          background = "$base";
          text = "$text";
          indicator = "$mauve";
          childBorder = "$overlay0";
        };
        urgent = {
          border = "$red";
          background = "$base";
          text = "$red";
          indicator = "$overlay0";
          childBorder = "$red";
        };
        placeholder = {
          border = "$overlay0";
          background = "$base";
          text = "$text";
          indicator = "$overlay0";
          childBorder = "$overlay0";
        };
        background = "$crust";
      };
      output."*" = let
        wallpaper-pkg = inputs.nix-wallpaper.packages."x86_64-linux".default.override {
          preset = "catppuccin-mocha-rainbow";
          width = 3840;
          height = 2160;
          logoSize = 42;
          backgroundColor = "#11111b";
        };
      in {
        scale = "1";
        background = "${wallpaper-pkg}/share/wallpapers/nixos-wallpaper.png fill";
      };
      keybindings = let
        mod = config.wayland.windowManager.sway.config.modifier;
        conf = config.wayland.windowManager.sway.config;
      in lib.mkOptionDefault {
        "${mod}+Shift+q" = null;
        "${mod}+Return" = null;
        "${mod}+c" = "kill";
        "${mod}+q" = "exec ${conf.terminal}";
        "${mod}+d" = ''
          exec rofi \
            -show drun \
            -modi drun \
            -scroll-method 0 \
            -drun-match-fields all \
            -drun-display-format "{name}" \
            -no-drun-show-actions \
            -terminal wezterm \
            -theme ~/.config/rofi/config/launcher.rasi
        '';
        "${mod}+Semicolon" = "exec swaylock";
        "XF86AudioMute" = "exec wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle";
        "XF86AudioLowerVolume" = "exec wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-";
        "XF86AudioRaiseVolume" = "exec wpctl set-volume -l 1.5 @DEFAULT_AUDIO_SINK@ 5%+";
        "XF86AudioPrev" = "exec playerctl previous";
        "XF86AudioPlay" = "exec playerctl play-pause";
        "XF86AudioNext" = "exec playerctl next";
        "XF86MonBrightnessDown" = "exec brightnessctl set 5%-";
        "XF86MonBrightnessUp" = "exec brightnessctl set 5%+";
        "${mod}+Print" = "exec sh -c 'grim - | wl-copy'";
        "${mod}+Shift+Print" = "exec sh -c 'slurp | grim -g - - | wl-copy'";
        "${mod}+Tab" = "exec ~/.config/sway/bin/dropterm.sh"; # TODO make this more better
      };
      input = {
        "type:touchpad" = {
          dwt = "disabled";
          natural_scroll = "enabled";
          click_method = "clickfinger";
        };
        "*" = {
          pointer_accel = "0.7";
          xkb_options = "compose:ralt";
        };
      };
      bars = [];
      startup = [
        { command = "xhost +SI:localuser:root"; always = true; }
      ];
      window = {
        titlebar = false;
        border = 1;
      };
    };
  };

  programs.waybar = {
    enable = true;
    systemd.enable = true;
    settings = [{
      "layer" = "top";
      "position" = "top";
      "modules-left" = [
        "sway/mode"
        "sway/workspaces"
        "custom/right-arrow-dark"
        "custom/right-arrow-light"
        "tray"
        "custom/right-arrow-dark"
        "custom/right-arrow-light"
        "idle_inhibitor"
        "custom/right-arrow-dark"

      ];
      "modules-center" = [
        "custom/left-arrow-dark"
        "clock#1"
        "custom/left-arrow-light"
        "custom/left-arrow-dark"
        "sway/window"
        "custom/right-arrow-dark"
        "custom/right-arrow-light"
        "clock#2"
        "custom/right-arrow-dark"
      ];
      "modules-right" = [
        "custom/left-arrow-dark"
        "wireplumber"
        "custom/left-arrow-light"
        "custom/left-arrow-dark"
        "memory"
        "custom/left-arrow-light"
        "custom/left-arrow-dark"
        "cpu"
        "custom/left-arrow-light"
        # "custom/left-arrow-dark"
        # "custom/ps"
        # "custom/left-arrow-light"
        "custom/left-arrow-dark"
        "battery"
        "custom/left-arrow-light"
        "custom/left-arrow-dark"
        "disk"
      ];

      "custom/left-arrow-dark" = {
        "format" = "";
        "tooltip" = false;
      };
      "custom/left-arrow-light" = {
        "format" = "";
        "tooltip" = false;
      };
      "custom/right-arrow-dark" = {
        "format" = "";
        "tooltip" = false;
      };
      "custom/right-arrow-light" = {
        "format" = "";
        "tooltip" = false;
      };

      "sway/workspaces" = {
        "disable-scroll" = true;
        "format" = "{name}";
      };

      "clock#1" = {
        "format" = "{:%m/%d}";
        "tooltip" = false;
      };
      "clock#2" = {
        "format" = "{:%H:%M}";
        "tooltip" = false;
      };
      "clock#3" ={
        "format" = "{:%m-%d}";
        "tooltip" = false;
      };
      "wireplumber" = {
        "format" = "{icon} {volume:2}%";
        "format-muted" = "󰖁 {volume}%";
        "format-icons" = [
          "󰕿"
          "󰖀"
          "󰕾"
          "󰕾"
        ];
        "scroll-step" = 5;
        "on-click" = "pamixer -t";
        "on-click-right" = "pavucontrol";
      };
      "memory" = {
        "interval" = 5;
        "format" = "󰍛 {used}GiB";
      };
      "cpu" = {
        "interval" = 5;
        "format" = "󰘚 {usage}%";
      };
      "battery" = {
        "states" = {
          "good" = 100;
          "warning" = 30;
          "critical" = 15;
        };
        "format" = "{icon}  {capacity}%";
        "format-icons" = [
          ""
          ""
          ""
          ""
          ""
        ];
      };
      "disk" = {
        "interval" = 5;
        "format" = " {percentage_used:2}%";
        "path" = "/mnt/fsroot";
      };
      "tray" = {
        "icon-size" = 24;
      };
      "sway/window" = {
        "max-length" = 50;
      };
      "idle_inhibitor" = {
        "format" = "{icon}";
        "format-icons" = {
          "activated" = "󰅶";
          "deactivated" = "󰾪";
        };
      };
      "custom/ps" = {
        "format" = "{icon} {}";
        "format-icons" = " ";
        "exec" = "ps aux --no-headers | wc -l";
        "interval" = 60;
      };
    }];
    style = ''
      @define-color base   #1e1e2e;
      @define-color mantle #181825;
      @define-color crust  #11111b;

      @define-color text     #cdd6f4;
      @define-color subtext0 #a6adc8;
      @define-color subtext1 #bac2de;

      @define-color surface0 #313244;
      @define-color surface1 #45475a;
      @define-color surface2 #585b70;

      @define-color overlay0 #6c7086;
      @define-color overlay1 #7f849c;
      @define-color overlay2 #9399b2;

      @define-color blue      #89b4fa;
      @define-color lavender  #b4befe;
      @define-color sapphire  #74c7ec;
      @define-color sky       #89dceb;
      @define-color teal      #94e2d5;
      @define-color green     #a6e3a1;
      @define-color yellow    #f9e2af;
      @define-color peach     #fab387;
      @define-color maroon    #eba0ac;
      @define-color red       #f38ba8;
      @define-color mauve     #cba6f7;
      @define-color pink      #f5c2e7;
      @define-color flamingo  #f2cdcd;
      @define-color rosewater #f5e0dc;

      * {
        font-size: 20px;
        font-family: 'FiraCode Nerd Font', monospace;
      }

      window#waybar {
        background: @crust;
        color: @text;
      }

      #custom-right-arrow-dark,
      #custom-left-arrow-dark {
        color: @base;
      }
      #custom-right-arrow-light,
      #custom-left-arrow-light {
        color: @crust;
        background: @base;
      }

      #workspaces,
      #mode,
      #clock.1,
      #clock.2,
      #window,
      #wireplumber,
      #memory,
      #cpu,
      #battery, 
      #disk,
      #tray,
      #idle_inhibitor {
        background: @base;
      }

      #mode {
         color: @red;
      }
      #workspaces button {
        padding: 0 2px;
        color: @text;
      }
      #workspaces button.focused,
      #workspaces button.active {
        color: @mauve;
      }
      #workspaces button:hover {
        box-shadow: inherit;
        text-shadow: inherit;
      }
      #workspaces button:hover {
        background: @base;
        border: @base;
      }

      #wireplumber {
        color: @sapphire;
      }
      #wireplumber.muted {
        color: @surface1;
      }
      #memory {
        color: @blue;
      }
      #cpu {
        color: @lavender;
      }
      #battery.good {
        color: @green;
      }
      #battery.warning {
        color: @yellow;
      }
      #battery.critical {
        color: @red;
      }
      #disk {
        color: @peach;
      }

      #clock,
      #wireplumber,
      #memory,
      #cpu,
      #battery,
      #disk {
        padding: 0 10px;
      }

      #tray {
        padding: 0 0 0 6;
      }

      #idle_inhibitor {
        padding: 0 14 0 10;
      }
      #idle_inhibitor.activated {
        color: @red;
      }
    '';
  };
  services.network-manager-applet.enable = true;

  programs.swaylock = {
    enable = true;
    package = pkgs.swaylock-effects;
    settings = {
      screenshots = true;
      clock = true;
      timestr = "%H:%M:%S";
      datestr = "%Y-%m-%d";
      indicator = true;
      indicator-radius = 350;
      indicator-thickness = 12;
      effect-blur = "8x5";
      ring-color = "cba6f7";
      ring-clear-color = "fab387";
      ring-ver-color = "74c7ec";
      ring-wrong-color = "f38ba8";
      key-hl-color = "45475a";
      bs-hl-color = "fab387";
      line-color = "00000000";
      line-clear-color = "00000000";
      line-caps-lock-color = "00000000";
      line-ver-color = "00000000";
      line-wrong-color = "00000000";
      inside-color = "00000000";
      inside-clear-color = "00000000";
      inside-caps-lock-color = "00000000";
      inside-ver-color = "00000000";
      inside-wrong-color = "00000000";
      separator-color = "00000000";
      text-color = "cba6f7";
      text-clear-color = "fab387";
      text-caps-lock-color = "f38ba8";
      text-ver-color = "74c7ec";
      text-wrong-color = "f38ba8";
      fade-in = 0.2;
      font = "Raleway";
    };
  };

  services.swayidle = {
    enable = true;
    timeouts = [
      { 
        timeout = 180;
        command = "${pkgs.swaylock-effects}/bin/swaylock --grace 20";
      }
      # {
      #   timeout = 150;
      #   command = "hyprctl dispatch dpms off";
      # }
    ];
    events = [
      # {
      #   event = "after-resume";
      #   command = "hyprctl dispatch dpms on";
      # }
      { 
        event = "before-sleep";
        command = "${pkgs.swaylock-effects}/bin/swaylock";
      }
    ];
  };

  services.mako = {
    enable = true;
    font = "Raleway 13";
    defaultTimeout = 5000;
    anchor = "bottom-right";
    width = 400;
    height = 400;
    backgroundColor = "#1e1e2e";
    textColor = "#cdd6f4";
    borderColor = "#cba6f7";
    progressColor = "#313244";
    extraConfig = ''
      [urgency=high]
      border-color=#f38ba8

      [urgency=low]
      border-color=#a6e3a1
    '';
  };

  services.udiskie = {
    enable = true;
    automount = false;
  };

  programs.emacs = {
    enable = true;
    extraPackages = epkgs: [ epkgs.vterm ];
  };

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

  programs.gh = {
    enable = true;
    gitCredentialHelper.enable = true;
  };

  programs.git = {
    enable = true;
    userName = "Ex-32";
    userEmail = "jenna@fligor.net";
    extraConfig = {
      init.defaultBranch = "main";
      safe.directory = "/etc/nixos";
    };
  };

  programs.fish = {
    enable = true;
    shellInit = ''
      set -g fish_greeting
    '';
    shellAliases = {
        cd = "z";
    };
    functions = {
      leak.body = ''
        fish -c "$argv &> /dev/null &"
      '';
      onExit = {
        onEvent = "fish_exit";
        body = "clear";
      };
    };
    plugins = [
      {
        name = "bang-bang";
        src = pkgs.fetchzip {
          url = "https://github.com/oh-my-fish/plugin-bang-bang/archive/master.zip";
          sha256 = "oPPCtFN2DPuM//c48SXb4TrFRjJtccg0YPXcAo0Lxq0=";
        };
      }
      {
        name = "bass";
        src = pkgs.fetchzip {
          url = "https://github.com/edc/bass/archive/master.zip";
          sha256 = "h6NM7BMFnFgyGL0rwiUq8UPYEDpnivhMjfHQJua06N8=";
        };
      }
    ];
  };

  programs.starship = {
    enable = true;
    # enableTransience = true;
    settings = {
      add_newline = false;
      palette = "catppuccin-mocha";
      format = lib.concatStrings [
        "$nix_shell" "$username" "$hostname" "$jobs" "$directory"
        "$fossil_branch" "$git_branch" "$git_commit" "$git_state" "$git_metrics"
        "$git_status" "$hg_branch" "$fill" "$shlvl" "$singularity" "$kubernetes"
        "$vcsh" "$pijul_channel" "$docker_context" "$package" "$c" "$cmake"
        "$cobol" "$daml" "$dart" "$deno" "$dotnet" "$elixir" "$elm" "$erlang"
        "$fennel" "$golang" "$guix_shell" "$haskell" "$haxe" "$helm" "$java"
        "$julia" "$kotlin" "$gradle" "$lua" "$nim" "$nodejs" "$ocaml" "$opa"
        "$perl" "$php" "$pulumi" "$purescript" "$python" "$raku" "$rlang" "$red"
        "$ruby" "$scala" "$swift" "$terraform" "$vlang" "$vagrant" "$zig" "$buf"
        "$conda" "$meson" "$spack" "$aws" "$gcloud" "$openstack" "$azure"
        "$env_var" "$crystal" "$container" "$memory_usage" "$battery" "$sudo"
        "$cmd_duration" "$time" "$shell" "$line_break" "$os" "$character"
      ];
      right_format = "$status";
      continuation_prompt = "▶▶ ";
      palettes.catppuccin-mocha = {
        rosewater = "#f5e0dc";
        flamingo = "#f2cdcd";
        pink = "#f5c2e7";
        mauve = "#cba6f7";
        red = "#f38ba8";
        maroon = "#eba0ac";
        peach = "#fab387";
        yellow = "#f9e2af";
        green = "#a6e3a1";
        teal = "#94e2d5";
        sky = "#89dceb";
        sapphire = "#74c7ec";
        blue = "#89b4fa";
        lavender = "#b4befe";
        text = "#cdd6f4";
        subtext1 = "#bac2de";
        subtext0 = "#a6adc8";
        overlay2 = "#9399b2";
        overlay1 = "#7f849c";
        overlay0 = "#6c7086";
        surface2 = "#585b70";
        surface1 = "#45475a";
        surface0 = "#313244";
        base = "#1e1e2e";
        mantle = "#181825";
        crust = "#11111b";
      };
      aws = {
        format = "[$symbol($profile)(\($region\))($duration )]($style) ";
        symbol = " ";
      };
      buf.symbol = " ";
      bun.format = "[$symbol($version)]($style) ";
      c = {
        symbol = " ";
        format = "[$symbol($version(-$name))]($style) ";
      };
      character = {
        success_symbol = "[󰘧](green bold)";
        error_symbol = "[󰘧](red bold)";
      };
      cmake.format = "[$symbol($version)]($style) ";
      cmd_duration.format = "[ $symbol$duration]($style) ";
      cobol.format = "[$symbol($version)]($style) ";
      conda = {
        symbol = " ";
        format = "[$symbol$environment]($style) ";
      };
      container = {
        symbol = "󰆧 ";
        format = "[$symbol\\[$name\\]]($style) ";
      };
      crystal.format = "[$symbol($version)]($style) ";
      daml.format = "[$symbol($version)]($style) ";
      dart = {
        symbol = " ";
        format = "[$symbol($version)]($style) ";
      };
      deno.format = "[$symbol($version)]($style) ";
      directory = {
        truncation_length = -1;
        read_only = " 󰌾";
        read_only_style = "red dimmed";
      };
      docker_context = {
        symbol = " ";
        format = "[$symbol$context]($style) ";
      };
      dotnet.format = "[$symbol($version)(󰓾 $tfm)]($style) ";
      elixir = {
        symbol = " ";
        format = "[$symbol($version \(OTP $otp_version\))]($style) ";
      };
      elm = {
        symbol = " ";
        format = "[$symbol($version)]($style) ";
      };
      erlang.format = "[$symbol($version)]($style) ";
      fennel.format = "[$symbol($version)]($style) ";
      fill = {
        symbol = " ";
        style = "";
      };
      fossil_branch = {
        symbol = " ";
        format = "[$symbol$branch]($style) ";
      };
      gcloud.format = "[$symbol$account(@$domain)(\($region\))]($style) ";
      git_branch = {
        symbol = " ";
        format = "[$symbol$branch]($style) ";
      };
      git_status.format = "([$all_status$ahead_behind]($style))";
      golang = {
        symbol = " ";
        format = "[$symbol($version)]($style) ";
      };
      gradle = {
        symbol = " ";
        format = "[$symbol($version)]($style) ";
      };
      guix_shell = {
        symbol = " ";
        format = "[$symbol]($style) ";
      };
      haskell = {
        symbol = " ";
        format = "[$symbol($version)]($style) ";
      };
      haxe = {
        symbol = "⌘ ";
        format = "[$symbol($version)]($style) ";
      };
      helm.format = "[$symbol($version)]($style) ";
      hg_branch = {
        symbol = " ";
        format = "[$symbol$branch]($style) ";
      };
      hostname = {
        ssh_symbol = " ";
        ssh_only = true;
        format = "@[$hostname$ssh_symbol]($style) ";
        style = "yellow bold";
      };
      java = {
        symbol = " ";
        format = "[$symbol($version)]($style) ";
      };
      jobs = {
        symbol = "";
        style = "yellow";
      };
      julia = {
        symbol = " ";
        format = "[$symbol($version)]($style) ";
      };
      kotlin = {
        symbol = " ";
        format = "[$symbol($version)]($style) ";
      };
      kubernetes = {
        format = "[$symbol$context( \($namespace\))]($style) ";
      };
      lua = {
        symbol = " ";
        format = "[$symbol($version)]($style) ";
      };
      memory_usage = {
        symbol = "󰍛 ";
        style = "red bold";
        format = "$symbol[$ram_pct]($style) ";
        threshold = 75;
        disabled = false;
      };
      meson = {
        symbol = "󰔷 ";
        format = "[$symbol$project]($style) ";
      };
      nim = {
        symbol = "󰆥 ";
        format = "[$symbol($version)]($style) ";
      };
      nix_shell = {
        heuristic = true;
        symbol = " ";
        format = "[$symbol$state( \($name\))]($style) ";
      };
      nodejs = {
        symbol = " ";
        format = "[$symbol($version)]($style) ";
      };
      ocaml = {
        symbol = " ";
        format = "[$symbol($version)(\($switch_indicator$switch_name\))]($style) ";
      };
      opa.format = "[$symbol($version)]($style) ";
      openstack.format = "[$symbol$cloud(\($project\))]($style) ";
      os = {
        disabled = true;
        symbols = {
          Alpaquita = " ";
          Alpine = " ";
          Amazon = "[ ](#FF9900)";
          Android = " ";
          Arch = "[ ](#1793D1)";
          Artix = " ";
          CentOS = " ";
          Debian = "[ ](#d70a53)";
          DragonFly = " ";
          Emscripten = " ";
          EndeavourOS = " ";
          Fedora = "[ ](#3c6eb4)";
          FreeBSD = "[ ](#AB2B28)";
          Garuda = "󰛓 ";
          Gentoo = "[ ](#61538D)";
          HardenedBSD = "󰞌 ";
          Illumos = "󰈸 ";
          Linux = " ";
          Mabox = " ";
          Macos = " ";
          Manjaro = "[ ](#34be5b)";
          Mariner = " ";
          MidnightBSD = " ";
          Mint = " ";
          NetBSD = " ";
          NixOS = " ";
          OpenBSD = "󰈺 ";
          openSUSE = " ";
          OracleLinux = "󰌷 ";
          Pop = " ";
          Raspbian = "[ ](#c51a4a)";
          Redhat = " ";
          RedHatEnterprise = " ";
          Redox = " ";
          Solus = "ﴱ ";
          SUSE = " ";
          Ubuntu = "[ ](#E95420)";
          Unknown = " ";
          Windows = "󰍲 ";
        };
      };
      package = {
        symbol = "󰏗 ";
        format = "[$symbol$version]($style) ";
      };
      perl.format = "[$symbol($version)]($style) ";
      php.format = "[$symbol($version)]($style) ";
      pijul_channel = {
        symbol = " ";
        format = "[$symbol$channel]($style) ";
      };
      pulumi.format = "[$symbol$stack]($style) ";
      purescript.format = "[$symbol($version)]($style) ";
      python = {
        symbol = " ";
        format = "[$symbol$pyenv_prefix($version)(\($virtualenv\))]($style) ";
      };
      raku.format = "[$symbol($version-$vm_version)]($style) ";
      red.format = "[$symbol($version)]($style) ";
      rlang = {
        symbol = "󰟔 ";
        format = "[$symbol($version)]($style) ";
      };
      ruby = {
        symbol = "";
        format = "[$symbol($version)]($style) ";
      };
      rust  = {
        symbol = "";
        format = "[$symbol($version)]($style) ";
      };
      scala = {
        symbol = "";
        format = "[$symbol($version)]($style) ";
      };
      shell = {
        format = " [$indicator]($style) ";
        bash_indicator = "bash";
        fish_indicator = "fish";
        powershell_indicator = "powershell";
        elvish_indicator = "elvish";
        tcsh_indicator = "tcsh";
        xonsh_indicator = "xonsh";
        unknown_indicator = "UNKNOWN SHELL";
        disabled = false;
      };
      spack = {
        symbol = "🅢 ";
        format = "[$symbol$environment]($style) ";
      };
      status = {
        symbol = "✘ ";
        sigint_symbol = "󱞨 ";
        not_executable_symbol = " ";
        not_found_symbol = "󰍉 ";
        signal_symbol = "󱐋 ";
        map_symbol = true;
        disabled = false;
      };
      sudo.format = "[as $symbol] ";
      swift.format = "[$symbol($version)]($style) ";
      terraform.format = "[$symbol$workspace]($style) ";
      time = {
        format = "[ $symbol$time]($style) ";
        time_format = "%H:%M";
        disabled = false;
      };
      username.format = "[ $user]($style)";
      vagrant.format = "[$symbol($version)]($style) ";
      vlang.format = "[$symbol($version)]($style) ";
      zig.format = "[$symbol($version)]($style) ";
    };
  };

  programs.zoxide.enable = true;
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  dconf = {
    enable = true;
  };

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

  qt = {
    enable = true;
    platformTheme = "gnome";
    style = {
      name = "kvantum-dark";
      package = pkgs.libsForQt5.qtstyleplugin-kvantum;
    };
  };

  home.file = {
    ".config/python3/startup.py".text = ''
      # borrowed from https://unix.stackexchange.com/a/704612

      # Enable custom ~/.python_history location on Python interactive console
      # Set PYTHONSTARTUP to this file on ~/.profile or similar for this to work
      # https://docs.python.org/3/using/cmdline.html#envvar-PYTHONSTARTUP
      # https://docs.python.org/3/library/readline.html#example
      # https://github.com/python/cpython/blob/main/Lib/site.py @ enablerlcompleter()
      # https://unix.stackexchange.com/a/675631/4919

      import atexit
      import os
      import readline
      import time


      def write_history(path):
          import os
          import readline
          try:
              os.makedirs(os.path.dirname(path), mode=0o700, exist_ok=True)
              readline.write_history_file(path)
          except OSError:
              pass


      history = os.path.join(os.environ.get('XDG_CACHE_HOME') or
                            os.path.expanduser('~/.cache'),
                            'python_history')
      try:
          readline.read_history_file(history)
      except FileNotFoundError:
          pass

      # Prevents creation of default history if custom is empty
      if readline.get_current_history_length() == 0:
          readline.add_history(f'# History created at {time.asctime()}')

      atexit.register(write_history, history)
      del (atexit, os, readline, time, history, write_history)
    '';
    ".config/npm/npmrc".text = ''
      prefix=''${XDG_DATA_HOME}/npm
      cache=''${XDG_CACHE_HOME}/npm
      init-module=''${XDG_CONFIG_HOME}/npm/config/npm-init.js
    '';
    ".config/latexmk/latexmkrc".text = ''
      $pdf_previewer = 'zathura';
      $latexmk = 'latexmk -interaction=nonstopmode';
    '';
  };

  programs.home-manager.enable = true;
  home.stateVersion = "23.05";
}

