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

    nix-darwin = {
      url = "github:LnL7/nix-darwin";
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
          ./systems/nixos-pc.nix

          ./nixos/appimage-binfmt.nix
          ./nixos/auth.nix
          ./nixos/base.nix
          ./nixos/bluetooth.nix
          ./nixos/console.nix
          ./nixos/desktop.nix
          ./nixos/distrobox.nix
          ./nixos/flipperzero.nix
          ./nixos/impermanence.nix
          ./nixos/jellyfin.nix
          ./nixos/locale.nix
          ./nixos/network.nix
          ./nixos/printing.nix
          ./nixos/shell.nix
          ./nixos/sound.nix
          ./nixos/ssh.nix
          ./nixos/steam.nix
          ./nixos/users.nix
          ./nixos/vial.nix
          ./nixos/x11.nix

          {
            nixpkgs.config.permittedInsecurePackages = [
              # FIXME: look to see if nheko is fixing upstream or else find new matrix client
              "olm-3.2.16"
            ];
          }

          {
            home-manager.users.jenna = {pkgs, ...}: {
              imports = [
                ./home-manager/base.nix
                ./home-manager/firefox.nix
                ./home-manager/fun.nix
                ./home-manager/games.nix
                ./home-manager/ghostty.nix
                ./home-manager/git.nix
                ./home-manager/impermanence.nix
                ./home-manager/kitty.nix
                ./home-manager/media.nix
                ./home-manager/neovim.nix
                ./home-manager/nix-index.nix
                ./home-manager/nushell.nix
                ./home-manager/obs-studio.nix
                ./home-manager/photos.nix
                ./home-manager/productivity.nix
                ./home-manager/socials.nix
                ./home-manager/spotify.nix
                ./home-manager/syncthing.nix
                ./home-manager/xdg.nix
                ./home-manager/xmonad.nix
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
          ./systems/nixbook.nix

          ./nixos/appimage-binfmt.nix
          ./nixos/auth.nix
          ./nixos/base.nix
          ./nixos/bluetooth.nix
          ./nixos/console.nix
          ./nixos/desktop.nix
          ./nixos/distrobox.nix
          ./nixos/flipperzero.nix
          ./nixos/impermanence.nix
          ./nixos/locale.nix
          ./nixos/network.nix
          ./nixos/printing.nix
          ./nixos/shell.nix
          ./nixos/sound.nix
          ./nixos/steam.nix
          ./nixos/tailscale.nix
          ./nixos/users.nix
          ./nixos/virt-manager.nix

          {
            nixpkgs.config.permittedInsecurePackages = [
              # FIXME: look to see if nheko is fixing upstream or else find new matrix client
              "olm-3.2.16"
            ];
          }

          {
            home-manager.users.jenna = {pkgs, ...}: {
              imports = [
                ./home-manager/base.nix
                ./home-manager/firefox.nix
                ./home-manager/fun.nix
                ./home-manager/games.nix
                ./home-manager/ghostty.nix
                ./home-manager/git.nix
                ./home-manager/hyprland.nix
                ./home-manager/impermanence.nix
                ./home-manager/media.nix
                ./home-manager/neovim.nix
                ./home-manager/nix-index.nix
                ./home-manager/nushell.nix
                ./home-manager/obs-studio.nix
                ./home-manager/productivity.nix
                ./home-manager/socials.nix
                ./home-manager/spotify.nix
                ./home-manager/syncthing.nix
                ./home-manager/xdg.nix
                ./home-manager/zed.nix
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

    darwinConfigurations."o7" = inputs.nix-darwin.lib.darwinSystem {
      system = "x86_64-darwin";
      specialArgs = {inherit inputs;};
      modules = [
        ./darwin/base.nix
        ./darwin/users.nix
        ./darwin/yabai.nix

        {
          home-manager.users.jenna = {pkgs, ...}: {
            imports = [
              ./home-manager/base.nix
              ./home-manager/firefox.nix
              ./home-manager/fun.nix
              ./home-manager/git.nix
              ./home-manager/kitty.nix
              ./home-manager/neovim.nix
              ./home-manager/nushell.nix
              ./home-manager/productivity.nix
              ./home-manager/socials.nix
              ./home-manager/spotify.nix
              ./home-manager/syncthing.nix
            ];
            allowedUnfree = [];
            home.packages = with pkgs; [
            ];
          };
        }
      ];
    };

    devShells = let
      forSystems = nixpkgs.lib.genAttrs [
        "aarch64-linux"
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
                xmonad_0_18_0
                xmonad-contrib
                taffybar
              ]))
          ];
        };
      });
  };
}
