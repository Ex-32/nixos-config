{
  description = "NixOS configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nur.url = "github:nix-community/NUR";
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    impermanence.url = "github:nix-community/impermanence";
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
          ./nixos/obs-studio.nix
          ./nixos/printing.nix
          ./nixos/puzzle.nix
          ./nixos/shell.nix
          ./nixos/sops.nix
          ./nixos/sound.nix
          ./nixos/ssh.nix
          ./nixos/tailscale.nix
          ./nixos/users.nix
          ./nixos/vial.nix

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
                ./home-manager/kdeconnect.nix
                ./home-manager/kitty.nix
                ./home-manager/media.nix
                ./home-manager/neovim.nix
                ./home-manager/niri.nix
                ./home-manager/nix-index.nix
                ./home-manager/photos.nix
                ./home-manager/productivity.nix
                ./home-manager/socials.nix
                ./home-manager/spotify.nix
                ./home-manager/syncthing.nix
                ./home-manager/xdg.nix
                ./home-manager/xonsh.nix
                ./home-manager/zellij.nix
                ./home-manager/zsh.nix
              ];
              home.packages = with pkgs; [
                bitwarden-desktop
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
          ./nixos/impermanence.nix
          ./nixos/locale.nix
          ./nixos/mullvad.nix
          ./nixos/network.nix
          ./nixos/obs-studio.nix
          ./nixos/printing.nix
          ./nixos/shell.nix
          ./nixos/sops.nix
          ./nixos/sound.nix
          ./nixos/tailscale.nix
          ./nixos/users.nix

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
                ./home-manager/productivity.nix
                ./home-manager/socials.nix
                ./home-manager/spotify.nix
                ./home-manager/syncthing.nix
                ./home-manager/xdg.nix
                ./home-manager/xonsh.nix
                ./home-manager/zellij.nix
                ./home-manager/zsh.nix
              ];
              home.packages = with pkgs; [
                bitwarden-desktop
                nyxt
                rclone
              ];
            };
          }
        ];
      };
    };
  };
}
