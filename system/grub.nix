{
  config,
  pkgs,
  lib,
  nixpkgs,
  ...
}: {
  boot.loader.efi = {
    canTouchEfiVariables = true;
    efiSysMountPoint = "/boot/efi";
  };
  boot.loader.grub = let
    font-path = "${pkgs.spleen}/share/fonts/misc/spleen-16x32.otf";
  in {
    enable = true;
    device = "nodev";
    efiSupport = true;
    useOSProber = true;
    enableCryptodisk = true;
    splashImage = null;

    # i'm honestly not sure if this is doing anything since i defined a custom
    # theme, but since the spleen packages is being used in that theme anyways,
    # it's not pulling in any additional packages, so if it ain't broke...
    font = "${font-path}";
    fontSize = 32;

    theme = let
      theme-path = "src/catppuccin-mocha-grub-theme";
    in
      pkgs.stdenv.mkDerivation rec {
        pname = "catppuccin-mocha-grub";
        version = "1.0.0";
        src = pkgs.fetchFromGitHub {
          owner = "catppuccin";
          repo = "grub";
          rev = "v${version}";
          hash = "sha256-/bSolCta8GCZ4lP0u5NVqYQ9Y3ZooYCNdTwORNvR7M0=";
        };
        nativeBuildInputs = with pkgs; [
          grub2
          spleen
        ];
        # HACK: using find 'n replace to change the font and to use relative
        # sizes for the menu options (the original didn't scale to 4k very well)
        prePatch = ''
          substituteInPlace ${theme-path}/theme.txt \
            --replace "Unifont Regular 16" "Spleen 16x32 Regular 32" \
            --replace "left = 50%-240" "left = 20%" \
            --replace "width = 480" "width = 60%"
        '';
        buildPhase = ''
          grub-mkfont --size 32 ${font-path} -o ${theme-path}/font.pf2
        '';
        installPhase = ''
          cp -r ${theme-path} $out
        '';
      };
  };
}
