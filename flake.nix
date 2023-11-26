{
  description = "NixOS configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    impermanence.url = "github:nix-community/impermanence";
    home-manager = { 
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    nix-wallpaper = {
      url = "github:lunik1/nix-wallpaper";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    blahaj = {
      url = "github:sioodmy/blahaj";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nur.url = "github:nix-community/NUR";
    spicetify-nix = {
      url = "github:the-argus/spicetify-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs @ { self, nixpkgs, home-manager, ... }: {
    nixosConfigurations = {
      "nixos-pc" = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          { networking.hostName = "nixos-pc"; }

          # impermanence setup
          inputs.impermanence.nixosModule

          # NUR overlay setup
          inputs.nur.nixosModules.nur
          { nixpkgs.overlays = [ inputs.nur.overlay ]; }

          # hardware configuration
          ./hardware/nixos-pc.nix
          ./hardware/nvidia.nix

          # system configuration
          ./system/base.nix
          ./system/grub.nix
          ./system/console.nix
          ./system/ssh.nix
          ./system/shell.nix
          ./system/users.nix
          ./system/network.nix
          ./system/desktop.nix
          ./system/sound.nix
          ./system/locale.nix
          ./system/appimage-binfmt.nix
          ./system/ecryptfs.nix

          # home-manager configuration
          home-manager.nixosModules.home-manager
          {
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              extraSpecialArgs = { inherit inputs; };
              users.jenna = { config, pkgs, lib, inputs, ... }: {
                imports = [
                  ./user/base.nix
                  ./user/fish.nix
                  ./user/neovim.nix
                  ./user/git.nix
                  ./user/python.nix
                  ./user/wezterm.nix
                  ./user/xdg.nix
                  ./user/spotify.nix
                ];
                home.packages = with pkgs; [
                  inputs.blahaj.packages.${pkgs.system}.default
                  _1password-gui
                  comma
                  discord
                  firefox-devedition
                  gparted
                  signal-desktop
                  tridactyl-native
                ];
              };
            };
          }
        ];
      };
      "nixbook" = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          { networking.hostName = "nixbook"; }

          # impermanence setup
          inputs.impermanence.nixosModule

          # NUR overlay setup
          inputs.nur.nixosModules.nur
          { nixpkgs.overlays = [ inputs.nur.overlay ]; }

          # hardware configuration
          inputs.nixos-hardware.nixosModules.framework-13th-gen-intel
          ./hardware/nixbook.nix

          # system configuration
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
          ./system/vial.nix
          ./system/desktop.nix

          # home-manager configuration 
          home-manager.nixosModules.home-manager
          {
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              extraSpecialArgs = { inherit inputs; };
              users.jenna = { config, pkgs, lib, inputs, ... }: {
                imports = [
                  ./user/base.nix
                  ./user/sway.nix
                  ./user/fish.nix
                  ./user/neovim.nix
                  ./user/git.nix
                  ./user/latex.nix
                  ./user/python.nix
                  ./user/wezterm.nix
                  ./user/xdg.nix
                  # ./user/firefox.nix
                  ./user/spotify.nix
                ];
                home.packages = with pkgs; [
                  inputs.blahaj.packages.${pkgs.system}.default
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
                  tridactyl-native
                ];
              };
            };
          }
        ];
      };
    };
  };
}
