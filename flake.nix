{
  description = "NixOS configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = { 
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    nix-wallpaper = {
      url = "github:lunik1/nix-wallpaper";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home-manager, ... }@inputs: {
    nixosConfigurations = {
      "nixos-pc" = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./hardware/nixos-pc.nix
          ./system/base.nix
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.jenna = import ./home.nix;
            home-manager.extraSpecialArgs = { inherit inputs; };
          }
        ];
      };
      "nixbook" = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          inputs.nixos-hardware.nixosModules.framework-13th-gen-intel
          ./hardware/nixbook.nix
          ./system/base.nix
          ./system/grub.nix
          ./system/console.nix
          ./system/shell.nix
          ./system/network.nix
          ./system/sound.nix
          ./system/users.nix
          ./system/auth.nix
          ./system/appimage-binfmt.nix
          ./system/locale.nix
          ./system/printing.nix
          ./system/steam.nix
          ./system/vial.nix
          ./system/desktop.nix
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.jenna = { config, pkgs, lib, inputs, ... }:
            {
              imports = [
                ./user/base.nix
                ./user/sway.nix
                ./user/fish.nix
                ./user/neovim.nix
                ./user/emacs.nix
                ./user/git.nix
                ./user/latex.nix
                ./user/python.nix
                ./user/wezterm.nix
                ./user/xdg.nix
              ];
              home.packages = with pkgs; [
                _1password-gui
                android-studio
                arduino
                comma
                discord
                firefox-devedition
                godot_4
                gparted
                obs-studio
                onlyoffice-bin
                rclone
                signal-desktop
              ];
            };
            home-manager.extraSpecialArgs = { inherit inputs; };
          }
        ];
      };
    };
  };
}
