{ config, pkgs, lib, inputs, ... }:

{
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
}
