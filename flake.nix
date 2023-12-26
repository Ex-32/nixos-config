{
  description = "NixOS configuration";

  inputs = {
    # unstable is my middle name B)
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    # it's stored in the aether 
    impermanence.url = "github:nix-community/impermanence";
    # dotfile maid 
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # laptops am i right...
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    # like the AUR, but flakier
    nur.url = "github:nix-community/NUR";
    # we come in peace
    nix-alien.url = "github:thiagokokada/nix-alien";
    # alias ls='neofetch'
    nix-wallpaper = {
      url = "github:lunik1/nix-wallpaper";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # spotify, but flakier
    spicetify-nix = {
      url = "github:the-argus/spicetify-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs @ { self, nixpkgs, home-manager, ... }: {
    nixosConfigurations = {
      "nixos-pc" = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = { inherit inputs; };
        modules = [
          { networking.hostName = "nixos-pc"; }

          # NUR overlay setup
          { 
            imports = [ inputs.nur.nixosModules.nur ];
            nixpkgs.overlays = [ inputs.nur.overlay ]; 
          }

          # hardware configuration
          ./hardware/nixos-pc.nix

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
          ./system/nix-alien.nix
          ./system/printing.nix
          ./system/shell.nix
          ./system/sound.nix
          ./system/ssh.nix
          ./system/users.nix
          ./system/vial.nix
          ./system/x11.nix
          ./system/kernel-latest.nix

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
                  ./user/i3.nix
                  ./user/neovim.nix
                  ./user/nix-index.nix
                  ./user/obs-studio.nix
                  ./user/picom.nix
                  ./user/python.nix
                  ./user/spotify.nix
                  ./user/wezterm.nix
                  ./user/xdg.nix
                ];
                home.packages = with pkgs; [
                  _1password-gui
                  comma
                  discord
                  element-desktop
                  endless-sky
                  ffmpeg
                  firefox-devedition
                  gparted
                  inkscape
                  onlyoffice-bin
                  prismlauncher
                  rawtherapee
                  signal-desktop
                  slack
                  superTuxKart
                  tridactyl-native
                  vlc
                ];
              };
            };
          }
        ];
      };
      "nixbook" = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = { inherit inputs; };
        modules = [
          { networking.hostName = "nixbook"; }

          # NUR overlay setup
          { 
            imports = [ inputs.nur.nixosModules.nur ];
            nixpkgs.overlays = [ inputs.nur.overlay ]; 
          }

          # hardware configuration
          ./hardware/nixbook.nix

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
          ./system/kernel-latest.nix

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
                  _1password-gui
                  comma
                  discord
                  element-desktop
                  endless-sky
                  ffmpeg
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
                  vlc
                ];
              };
            };
          }
        ];
      };
    };
  };
}
