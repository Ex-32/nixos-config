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
      "zion" = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = {inherit inputs;};
        modules = [
          ./systems/zion.nix

          ./nixos/appimage-binfmt.nix
          ./nixos/auth.nix
          ./nixos/base.nix
          ./nixos/bluetooth.nix
          ./nixos/ccc.nix
          ./nixos/console.nix
          ./nixos/desktop.nix
          ./nixos/distrobox.nix
          ./nixos/flipperzero.nix
          ./nixos/impermanence.nix
          ./nixos/locale.nix
          ./nixos/mullvad.nix
          ./nixos/network.nix
          ./nixos/printing.nix
          ./nixos/puzzle.nix
          ./nixos/shell.nix
          ./nixos/sound.nix
          ./nixos/ssh.nix
          ./nixos/steam.nix
          ./nixos/tailscale.nix
          ./nixos/users.nix
          ./nixos/vial.nix
          ./nixos/homelab-smb.nix

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
                ./home-manager/emacs.nix
                ./home-manager/firefox.nix
                ./home-manager/fun.nix
                ./home-manager/games.nix
                ./home-manager/git.nix
                ./home-manager/impermanence.nix
                ./home-manager/julia.nix
                ./home-manager/kitty.nix
                ./home-manager/media.nix
                ./home-manager/neovim.nix
                ./home-manager/niri.nix
                ./home-manager/nix-index.nix
                ./home-manager/obs-studio.nix
                ./home-manager/photos.nix
                ./home-manager/productivity.nix
                ./home-manager/socials.nix
                ./home-manager/spotify.nix
                ./home-manager/syncthing.nix
                ./home-manager/xdg.nix
                ./home-manager/zellij.nix
                ./home-manager/zsh.nix
              ];
              local.lutris.enable = true;
              allowedUnfree = ["1password"];
              home.packages = with pkgs; [
                _1password-gui
                gparted
                nyxt
                qbittorrent
              ];
            };
          }
        ];
      };
      "reason" = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = {inherit inputs;};
        modules = [
          ./systems/reason.nix

          ./nixos/appimage-binfmt.nix
          ./nixos/auth.nix
          ./nixos/base.nix
          ./nixos/bluetooth.nix
          ./nixos/console.nix
          ./nixos/desktop.nix
          ./nixos/distrobox.nix
          ./nixos/flipperzero.nix
          ./nixos/homelab-smb.nix
          ./nixos/impermanence.nix
          ./nixos/locale.nix
          ./nixos/mullvad.nix
          ./nixos/network.nix
          ./nixos/printing.nix
          ./nixos/shell.nix
          ./nixos/sound.nix
          ./nixos/steam.nix
          ./nixos/tailscale.nix
          ./nixos/users.nix
          ./nixos/vial.nix

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
                ./home-manager/emacs.nix
                ./home-manager/firefox.nix
                ./home-manager/fun.nix
                ./home-manager/games.nix
                ./home-manager/git.nix
                ./home-manager/impermanence.nix
                ./home-manager/julia.nix
                ./home-manager/kitty.nix
                ./home-manager/media.nix
                ./home-manager/neovim.nix
                ./home-manager/niri.nix
                ./home-manager/nix-index.nix
                ./home-manager/obs-studio.nix
                ./home-manager/productivity.nix
                ./home-manager/socials.nix
                ./home-manager/spotify.nix
                ./home-manager/syncthing.nix
                ./home-manager/xdg.nix
                ./home-manager/zellij.nix
                ./home-manager/zsh.nix
              ];
              allowedUnfree = ["1password"];
              home.packages = with pkgs; [
                _1password-gui
                gparted
                nyxt
                rclone
              ];
            };
          }
        ];
      };
    };

    darwinConfigurations."kiroshi" = inputs.nix-darwin.lib.darwinSystem {
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
              ./home-manager/productivity.nix
              ./home-manager/socials.nix
              ./home-manager/spotify.nix
              ./home-manager/syncthing.nix
              ./home-manager/zsh.nix
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
              ]))
          ];
        };
      });
  };
}
