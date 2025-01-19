{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  programs.starship = let
    def-false = lib.mkDefault false;
  in {
    enable = true;
    enableBashIntegration = def-false;
    enableFishIntegration = def-false;
    enableIonIntegration = def-false;
    enableNushellIntegration = def-false;
    enableZshIntegration = def-false;
    settings = {
      aws = {
        symbol = "  ";
      };
      buf = {
        symbol = " ";
      };
      c = {
        symbol = " ";
      };
      conda = {
        symbol = " ";
      };
      crystal = {
        symbol = " ";
      };
      dart = {
        symbol = " ";
      };
      directory = {
        read_only = " 󰌾";
      };
      docker_context = {
        symbol = " ";
      };
      elixir = {
        symbol = " ";
      };
      elm = {
        symbol = " ";
      };
      fennel = {
        symbol = " ";
      };
      fossil_branch = {
        symbol = " ";
      };
      git_branch = {
        symbol = " ";
      };
      git_commit = {
        tag_symbol = "  ";
      };
      golang = {
        symbol = " ";
      };
      gradle = {
        symbol = " ";
      };
      guix_shell = {
        symbol = " ";
      };
      haskell = {
        symbol = " ";
      };
      haxe = {
        symbol = " ";
      };
      hg_branch = {
        symbol = " ";
      };
      hostname = {
        ssh_symbol = " ";
      };
      java = {
        symbol = " ";
      };
      julia = {
        symbol = " ";
      };
      kotlin = {
        symbol = " ";
      };
      lua = {
        symbol = " ";
      };
      memory_usage = {
        symbol = "󰍛 ";
      };
      meson = {
        symbol = "󰔷 ";
      };
      nim = {
        symbol = "󰆥 ";
      };
      nix_shell = {
        symbol = " ";
      };
      nodejs = {
        symbol = " ";
      };
      ocaml = {
        symbol = " ";
      };
      os = {
        symbols = {
          AlmaLinux = " ";
          Alpaquita = " ";
          Alpine = "[ ](#0d597f)";
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
          Kali = " ";
          Linux = " ";
          Mabox = " ";
          Macos = " ";
          Manjaro = "[ ](#34be5b)";
          Mariner = " ";
          MidnightBSD = " ";
          Mint = "[ ](#87cf3e)";
          NetBSD = " ";
          NixOS = "[ ](#7EBAE4)";
          OpenBSD = "󰈺 ";
          OracleLinux = "󰌷 ";
          Pop = " ";
          Raspbian = "[ ](#c51a4a)";
          RedHatEnterprise = "[ ](#ee0000)";
          Redhat = "[ ](#ee0000)";
          Redox = "󰀘 ";
          RockyLinux = " ";
          SUSE = " ";
          Solus = "󰠳 ";
          Ubuntu = "[ ](#E95420)";
          Unknown = " ";
          Void = " ";
          Windows = "[󰍲 ](#357EC7)";
          openSUSE = " ";
        };
      };
      package = {
        symbol = "󰏗 ";
      };
      perl = {
        symbol = " ";
      };
      php = {
        symbol = " ";
      };
      pijul_channel = {
        symbol = " ";
      };
      python = {
        symbol = " ";
      };
      rlang = {
        symbol = "󰟔 ";
      };
      ruby = {
        symbol = " ";
      };
      rust = {
        symbol = "󱘗 ";
      };
      scala = {
        symbol = " ";
      };
      swift = {
        symbol = " ";
      };
      zig = {
        symbol = " ";
      };
    };
  };
}
