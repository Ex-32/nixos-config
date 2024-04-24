{
  description = "NixOS configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nur.url = "github:nix-community/NUR";
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    impermanence.url = "github:nix-community/impermanence";
    nix-alien.url = "github:thiagokokada/nix-alien";

    nix-wallpaper = {
      url = "github:lunik1/nix-wallpaper";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    spicetify-nix = {
      url = "github:the-argus/spicetify-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs @ {
    self,
    nixpkgs,
    home-manager,
    ...
  }: {
    nixosConfigurations = {
      "nixos-pc" = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = {inherit inputs;};
        modules = [
          {networking.hostName = "nixos-pc";}

          # hardware configuration
          ./systems/nixos-pc.nix

          # system configuration
          ./lib/appimage-binfmt.nix
          ./lib/auth.nix
          ./lib/base.nix
          ./lib/console.nix
          ./lib/desktop.nix
          ./lib/distrobox.nix
          ./lib/ecryptfs.nix
          ./lib/grub.nix
          ./lib/impermanence.nix
          ./lib/kernel-latest.nix
          ./lib/locale.nix
          ./lib/network.nix
          ./lib/nix-alien.nix
          ./lib/printing.nix
          ./lib/shell.nix
          ./lib/sound.nix
          ./lib/ssh.nix
          ./lib/steam.nix
          ./lib/users.nix
          ./lib/vial.nix
          ./lib/x11.nix
          ./lib/flipperzero.nix

          # home-manager configuration
          {
            home-manager.users.jenna = {pkgs, ...}: {
              imports = [
                ./home-manager/base.nix
                ./home-manager/fish.nix
                ./home-manager/git.nix
                ./home-manager/i3.nix
                ./home-manager/neovim.nix
                ./home-manager/nix-index.nix
                ./home-manager/picom.nix
                ./home-manager/python.nix
                ./home-manager/xdg.nix
                ./home-manager/kitty.nix
                ./home-manager/syncthing.nix
                ./home-manager/yazi.nix

                ./home-manager/apps/firefox.nix
                ./home-manager/apps/games.nix
                ./home-manager/apps/media.nix
                ./home-manager/apps/obs-studio.nix
                ./home-manager/apps/productivity.nix
                ./home-manager/apps/socials.nix
                ./home-manager/apps/spotify.nix
              ];
              local.lutris.enable = true;
              allowedUnfree = ["1password"];
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
        specialArgs = {inherit inputs;};
        modules = [
          {networking.hostName = "nixbook";}

          # hardware configuration
          ./systems/nixbook.nix

          # system configuration
          ./lib/appimage-binfmt.nix
          ./lib/auth.nix
          ./lib/base.nix
          ./lib/bluetooth.nix
          ./lib/console.nix
          ./lib/desktop.nix
          ./lib/distrobox.nix
          ./lib/docker.nix
          ./lib/flipperzero.nix
          ./lib/grub.nix
          ./lib/impermanence.nix
          ./lib/kernel-latest.nix
          ./lib/locale.nix
          ./lib/network.nix
          ./lib/nix-alien.nix
          ./lib/printing.nix
          ./lib/shell.nix
          ./lib/sound.nix
          ./lib/steam.nix
          ./lib/users.nix
          ./lib/vial.nix

          # home-manager configuration
          {
            home-manager.users.jenna = {pkgs, ...}: {
              imports = [
                ./home-manager/base.nix
                ./home-manager/fish.nix
                ./home-manager/git.nix
                ./home-manager/kitty.nix
                ./home-manager/latex.nix
                ./home-manager/neovim.nix
                ./home-manager/nix-index.nix
                ./home-manager/python.nix
                ./home-manager/sway.nix
                ./home-manager/syncthing.nix
                ./home-manager/xdg.nix
                ./home-manager/yazi.nix

                ./home-manager/apps/firefox.nix
                ./home-manager/apps/fun.nix
                ./home-manager/apps/games.nix
                ./home-manager/apps/media.nix
                ./home-manager/apps/obs-studio.nix
                ./home-manager/apps/productivity.nix
                ./home-manager/apps/socials.nix
                ./home-manager/apps/spotify.nix
              ];
              allowedUnfree = ["1password"];
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
