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
    # nix-doom-emacs = {
    #   url = "github:nix-community/nix-doom-emacs";
    #   inputs.nixpkgs.follows = "nixpkgs";
    # };
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
          ./system/appimage-binfmt.nix
          ./system/base.nix
          ./system/console.nix
          ./system/desktop.nix
          ./system/ecryptfs.nix
          ./system/grub.nix
          ./system/impermanence.nix
          ./system/locale.nix
          ./system/network.nix
          ./system/printing.nix
          ./system/shell.nix
          ./system/sound.nix
          ./system/ssh.nix
          ./system/users.nix
          ./system/vial.nix
          ./system/x11.nix

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
                  ./user/git.nix
                  ./user/neovim.nix
                  ./user/nix-index.nix
                  ./user/picom.nix
                  ./user/python.nix
                  ./user/spotify.nix
                  ./user/wezterm.nix
                  ./user/xdg.nix
                  ./user/xmonad.nix
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
          ./hardware/grub-patch.nix

          # system configuration
          ./system/appimage-binfmt.nix
          ./system/auth.nix
          ./system/base.nix 
          ./system/console.nix
          ./system/desktop.nix
          ./system/grub.nix
          ./system/impermanence.nix
          ./system/locale.nix
          ./system/network.nix
          ./system/printing.nix
          ./system/shell.nix
          ./system/sound.nix
          ./system/users.nix
          ./system/vial.nix

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
                  ./user/git.nix
                  ./user/latex.nix
                  ./user/neovim.nix
                  ./user/nix-index.nix
                  ./user/python.nix
                  ./user/spotify.nix
                  ./user/sway.nix
                  ./user/wezterm.nix
                  ./user/xdg.nix
                  ./user/obs-studio.nix
                ];
                home.packages = with pkgs; [
                  inputs.blahaj.packages.${pkgs.system}.default
                  _1password-gui
                  comma
                  discord
                  element-desktop
                  endless-sky
                  firefox-devedition
                  gparted
                  inkscape
                  onlyoffice-bin
                  rawtherapee
                  rclone
                  signal-desktop
                  slack
                  superTuxKart
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
