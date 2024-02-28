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
    nixos-hardware.url = "github:NixOS/nixos-hardware";
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

          # hardware configuration
          ./hardware/nixos-pc.nix

          # system configuration
          ./system/appimage-binfmt.nix
          ./system/auth.nix
          ./system/base.nix
          ./system/console.nix
          ./system/desktop.nix
          ./system/distrobox.nix
          ./system/ecryptfs.nix
          ./system/grub.nix
          ./system/impermanence.nix
          ./system/kernel-latest.nix
          ./system/locale.nix
          ./system/network.nix
          ./system/nix-alien.nix
          ./system/printing.nix
          ./system/shell.nix
          ./system/sound.nix
          ./system/ssh.nix
          ./system/steam.nix
          ./system/users.nix
          ./system/vial.nix
          ./system/x11.nix

          # home-manager configuration
          {
            home-manager.users.jenna = { pkgs, ... }: {
              imports = [
                ./user/base.nix
                ./user/fish.nix
                ./user/git.nix
                ./user/i3.nix
                ./user/neovim.nix
                ./user/nix-index.nix
                ./user/picom.nix
                ./user/python.nix
                ./user/wezterm.nix
                ./user/xdg.nix

                ./user/apps/games.nix
                ./user/apps/floorp.nix
                ./user/apps/media.nix
                ./user/apps/obs-studio.nix
                ./user/apps/productivity.nix
                ./user/apps/socials.nix
                ./user/apps/spotify.nix
              ];
              home.packages = with pkgs; [
                _1password-gui
                gparted
              ];
            };
          }
        ];
      };
      "nixbook" = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = { inherit inputs; };
        modules = [
          { networking.hostName = "nixbook"; }

          # hardware configuration
          ./hardware/nixbook.nix

          # system configuration
          ./system/appimage-binfmt.nix
          ./system/auth.nix
          ./system/base.nix
          ./system/bluetooth.nix
          ./system/console.nix
          ./system/desktop.nix
          ./system/distrobox.nix
          ./system/docker.nix
          ./system/grub.nix
          ./system/impermanence.nix
          ./system/kernel-latest.nix
          ./system/locale.nix
          ./system/network.nix
          ./system/printing.nix
          ./system/shell.nix
          ./system/sound.nix
          ./system/users.nix
          ./system/vial.nix

          # FIXME: currently obsidian fails to build without this, but i'd
          # prefer not to have insecure packages as part of my global state
          {
            nixpkgs.config.permittedInsecurePackages = [
              "electron-25.9.0"
            ];
          }

          # home-manager configuration
          {
            home-manager.users.jenna = { pkgs, ... }: {
              imports = [
                ./user/base.nix
                ./user/fish.nix
                ./user/git.nix
                ./user/latex.nix
                ./user/neovim.nix
                ./user/nix-index.nix
                ./user/python.nix
                ./user/sway.nix
                ./user/wezterm.nix
                ./user/xdg.nix

                ./user/apps/games.nix
                ./user/apps/media.nix
                ./user/apps/productivity.nix
                ./user/apps/socials.nix
                ./user/apps/spotify.nix
                ./user/apps/obs-studio.nix
                ./user/apps/floorp.nix
              ];
              home.packages = with pkgs; [
                _1password-gui
                gparted
                rclone
              ];
            };
          }
        ];
      };
    };
  };
}
