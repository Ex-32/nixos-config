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

    unstraightened = {
      url = "github:marienz/nix-doom-emacs-unstraightened";
      inputs.nixpkgs.follows = "";
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
          ./lib/flipperzero.nix
          ./lib/impermanence.nix
          ./lib/locale.nix
          ./lib/network.nix
          ./lib/printing.nix
          ./lib/shell.nix
          ./lib/sound.nix
          ./lib/ssh.nix
          ./lib/steam.nix
          ./lib/users.nix
          ./lib/vial.nix
          ./lib/x11.nix

          # home-manager configuration
          {
            home-manager.users.jenna = {pkgs, ...}: {
              imports = [
                ./home-manager/base.nix
                ./home-manager/emacs.nix
                ./home-manager/firefox.nix
                ./home-manager/fun.nix
                ./home-manager/games.nix
                ./home-manager/git.nix
                ./home-manager/impermanence.nix
                ./home-manager/kitty.nix
                ./home-manager/media.nix
                ./home-manager/neovim.nix
                ./home-manager/nix-index.nix
                ./home-manager/obs-studio.nix
                ./home-manager/photos.nix
                ./home-manager/productivity.nix
                ./home-manager/socials.nix
                ./home-manager/spotify.nix
                ./home-manager/syncthing.nix
                ./home-manager/xdg.nix
                ./home-manager/xmonad.nix
                ./home-manager/xonsh.nix
              ];
              local.lutris.enable = true;
              allowedUnfree = ["1password"];
              home.packages = with pkgs; [
                _1password-gui
                keepassxc
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
          ./lib/flipperzero.nix
          ./lib/impermanence.nix
          ./lib/locale.nix
          ./lib/network.nix
          ./lib/printing.nix
          ./lib/shell.nix
          ./lib/sound.nix
          ./lib/steam.nix
          ./lib/users.nix

          # home-manager configuration
          {
            home-manager.users.jenna = {pkgs, ...}: {
              imports = [
                ./home-manager/base.nix
                ./home-manager/emacs.nix
                ./home-manager/firefox.nix
                ./home-manager/fun.nix
                ./home-manager/games.nix
                ./home-manager/git.nix
                ./home-manager/hyprland.nix
                ./home-manager/impermanence.nix
                ./home-manager/kitty.nix
                ./home-manager/media.nix
                ./home-manager/neovim.nix
                ./home-manager/nix-index.nix
                ./home-manager/obs-studio.nix
                ./home-manager/productivity.nix
                ./home-manager/socials.nix
                ./home-manager/spotify.nix
                ./home-manager/syncthing.nix
                ./home-manager/xdg.nix
                ./home-manager/xonsh.nix
              ];
              allowedUnfree = ["1password"];
              home.packages = with pkgs; [
                _1password-gui
                keepassxc
                gparted
                rclone
              ];
            };
          }
        ];
      };
    };

    devShells = let
      forSystems = nixpkgs.lib.genAttrs [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];

      nixpkgsFor = forSystems (system: import nixpkgs {inherit system;});
    in
      forSystems (system: let
        pkgs = nixpkgsFor.${system};
      in {
        default = pkgs.mkShell {
          packages = [
            (pkgs.haskellPackages.ghcWithPackages (hpkgs:
              with hpkgs; [
                xmonad
                xmonad-contrib
                taffybar
              ]))
          ];
        };
      });
  };
}
