{ config, pkgs, lib, ... }:
let
  flake-compat = builtins.fetchTarball {
    url = "https://github.com/edolstra/flake-compat/archive/master.tar.gz";
    sha256 = "1prd9b1xx8c0sfwnyzkspplh30m613j42l1k789s521f4kv4c2z2";
  };
  spicetify-nix =
    (import flake-compat {
      src = builtins.fetchTarball {
        url = "https://github.com/the-argus/spicetify-nix/archive/master.zip";
        sha256 = "0szlf5264kvyqz3rm27jjh7kbxldz078939267c9rpin45dadyiv";
      };
    }).defaultNix;
  spicePkgs = spicetify-nix.packages.${pkgs.system}.default;
in
{
  imports = [ spicetify-nix.homeManagerModule ];

  manual.manpages.enable = false;

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
    bacon
    brightnessctl
    cargo
    cargo-audit
    cargo-cache
    cargo-edit
    cargo-flamegraph
    cargo-generate
    cargo-update
    cargo-watch
    (catppuccin-kvantum.override {
      variant = "Mocha";
      accent  = "Mauve";
    })
    clang
    clippy
    discord
    dconf # needed by home-manager gtk config
    easyeffects
    gh
    gimp-with-plugins
    (nerdfonts.override {
      fonts = [
        "FiraCode"
        "NerdFontsSymbolsOnly"
      ];
    })
    nomacs
    obs-studio
    obsidian
    onlyoffice-bin
    playerctl
    prismlauncher
    rofi-wayland
    rust-analyzer
    rustfmt
    starship
    swayidle
    texlab
    texlive.combined.scheme-medium
    vivaldi
    xdg-utils
    zathura
    zoxide
  ];

  wayland.windowManager.sway = {
    enable = true;
    config = rec {
      modifier = "Mod4";
      terminal = "wezterm";
      gaps.inner = 10;
      output."*".scale = "1";
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
	  };
	  input."*" = {
		natural_scroll = "enabled";
		pointer_accel = "0.7";
		click_method = "clickfinger";
	  };
	  bars = [];
	};
  };

  programs.waybar = {
  	enable = true;
  	systemd.enable = true;
  };

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
        timeout = 120;
        command = "${pkgs.swaylock-effects}/bin/swaylock --grace 10";
      }
      # {
      #   timeout = 150;
      #   command = "hyprctl dispatch dpms off";
      # }
    ];
    events = [
      # {
      #   event = "resume";
      #   command = "hyprctl dispatch dpms on";
      # }
      { 
        event = "before-sleep";
        command = "${pkgs.swaylock-effects}/bin/swaylock";
      }
    ];
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
      config.font_size = 13
      config.hide_mouse_cursor_when_typing = false

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

  programs.gh = {
    enable = true;
    enableGitCredentialHelper = true;
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
      plasma.body = ''
        cd; or return 1
        clear; or return 1
        set -gx DESKTOP_SESSION plasma
        exec startx (which startplasma-x11) &>/dev/null
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
        "$os" "$username" "$hostname" "$jobs" "$directory" "$fossil_branch"
        "$git_branch" "$git_commit" "$git_state" "$git_metrics" "$git_status"
        "$hg_branch" "$fill" "$shlvl" "$singularity" "$kubernetes" "$vcsh"
        "$pijul_channel" "$docker_context" "$package" "$c" "$cmake" "$cobol"
        "$daml" "$dart" "$deno" "$dotnet" "$elixir" "$elm" "$erlang" "$fennel"
        "$golang" "$guix_shell" "$haskell" "$haxe" "$helm" "$java" "$julia"
        "$kotlin" "$gradle" "$lua" "$nim" "$nodejs" "$ocaml" "$opa" "$perl"
        "$php" "$pulumi" "$purescript" "$python" "$raku" "$rlang" "$red" "$ruby"
        "$scala" "$swift" "$terraform" "$vlang" "$vagrant" "$zig" "$buf"
        "$nix_shell" "$conda" "$meson" "$spack" "$aws" "$gcloud" "$openstack"
        "$azure" "$env_var" "$crystal" "$container" "$memory_usage" "$battery"
        "$sudo" "$cmd_duration" "$time" "$line_break" "$character"
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
        format = "[$symbol\[$name\]]($style) ";
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
        read_only = " ";
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
      gradle.format = "[$symbol($version)]($style) ";
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
      kotlin.format = "[$symbol($version)]($style) ";
      kubernetes = {
        format = "[$symbol$context( \($namespace\))]($style) ";
      };
      lua = {
        symbol = " ";
        format = "[$symbol($version)]($style) ";
      };
      memory_usage = {
        symbol = " ";
        style = "red bold";
        format = "$symbol[$ram_pct]($style) ";
        threshold = 75;
        disabled = false;
      };
      meson = {
        symbol = "喝 ";
        format = "[$symbol$project]($style) ";
      };
      nim = {
        symbol = " ";
        format = "[$symbol($version)]($style) ";
      };
      nix_shell = {
        symbol = " ";
        format = "[$symbol$state( \($name\))]($style) ";
      };
      nodejs = {
        symbol = " ";
        format = "[$symbol($version)]($style) ";
      };
      ocaml.format = "[$symbol($version)(\($switch_indicator$switch_name\))]($style) ";
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
          Garuda = "﯑ ";
          Gentoo = "[ ](#61538D)";
          HardenedBSD = "ﲊ ";
          Illumos = " ";
          Linux = " ";
          Mabox = " ";
          Macos = " ";
          Manjaro = "[ ](#34be5b)";
          Mariner = " ";
          MidnightBSD = " ";
          Mint = " ";
          NetBSD = " ";
          NixOS = " ";
          OpenBSD = " ";
          openSUSE = " ";
          OracleLinux = " ";
          Pop = " ";
          Raspbian = "[ ](#c51a4a)";
          Redhat = " ";
          RedHatEnterprise = " ";
          Redox = " ";
          Solus = "ﴱ ";
          SUSE = " ";
          Ubuntu = "[ ](#E95420)";
          Unknown = " ";
          Windows = " ";
        };
      };
      package = {
        symbol = " ";
        format = "[$symbol$version]($style) ";
      };
      perl.format = "[$symbol($version)]($style) ";
      php.format = "[$symbol($version)]($style) ";
      pijul_channel = {
        symbol = "🪺 ";
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
        symbol = "ﳒ";
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
        not_executable_symbol = "🛇 ";
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
  programs.direnv.enable = true;

  programs.spicetify = {
    enable = true;
    theme = spicePkgs.themes.catppuccin-mocha;
    colorScheme = "mauve";
    enabledExtensions = with spicePkgs.extensions; [
      autoSkipVideo
      autoSkipExplicit
      shuffle
      hidePodcasts
    ];
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

  home.pointerCursor = {
    name = "Catppuccin-Mocha-Mauve-Cursors";
    package = pkgs.catppuccin-cursors;
    size = 48;
    x11 = {
      enable = true;
      defaultCursor = "Catppuccin-Mocha-Mauve-Cursors";
    };
  };

  qt = {
  	enable = true;
  	platformTheme = "qtct";
  	style = {
      name = "kvantum-dark";
      package = pkgs.libsForQt5.qtstyleplugin-kvantum;
    };
  };

  programs.home-manager.enable = true;
  home.stateVersion = "23.05";
}
