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
      # "nixos-pc" = nixpkgs.lib.nixosSystem {
      #   system = "x86_64-linux";
      #   modules = [
      #     ./hardware/nixos-pc.nix
      #     ./system.nix
      #     home-manager.nixosModules.home-manager
      #     {
      #       home-manager.useGlobalPkgs = true;
      #       home-manager.useUserPackages = true;
      #       home-manager.users.jenna = import ./home.nix;
      #       # Optionally, use home-manager.extraSpecialArgs to pass
      #       # arguments to home.nix
      #     }
      #   ];
      # };
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
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.jenna = import ./home.nix;
            # home-manager.extraSpecialArgs = { inherit inputs; };
          }
        ];
      };
    };
  };
}
