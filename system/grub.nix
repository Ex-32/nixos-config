{ config, pkgs, lib, nixpkgs, ... }:

{
  boot.loader.efi = {
    canTouchEfiVariables = true;
    efiSysMountPoint = "/boot/efi";
  };
  boot.loader.grub = {
    enable = true;
    device = "nodev";
    efiSupport = true;
    enableCryptodisk = true;
    splashImage = null;
    font = "${pkgs.spleen}/share/fonts/misc/spleen-16x32.otf";
    fontSize = 32;
    theme = let
      git-rev = "803c5df0e83aba61668777bb96d90ab8f6847106";
      path = "src/catppuccin-mocha-grub-theme";
    in pkgs.stdenv.mkDerivation {
      pname = "catppuccin-mocha-grub";
      version = "${git-rev}";
      src = pkgs.fetchFromGitHub {
        owner = "catppuccin";
        repo = "grub";
        rev = "${git-rev}";
        hash = "sha256-/bSolCta8GCZ4lP0u5NVqYQ9Y3ZooYCNdTwORNvR7M0=";
      };
      nativeBuildInputs = with pkgs; [
        grub2
        spleen
      ];
      prePatch = ''
        substituteInPlace ${path}/theme.txt --replace "Unifont Regular 16" "Spleen 16x32 Regular 32"
        substituteInPlace  ${path}/theme.txt --replace "left = 50%-240" "left = 20%"
        substituteInPlace ${path}/theme.txt --replace "width = 480" "width = 60%"
      '';
      buildPhase = ''
        grub-mkfont --size 32 ${pkgs.spleen}/share/fonts/misc/spleen-16x32.otf -o ${path}/font.pf2
      '';
      installPhase = ''
        cp -r ${path} $out
      '';
    };
  };
}
