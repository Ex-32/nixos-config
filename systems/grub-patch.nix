{
  config,
  pkgs,
  lib,
  nixpkgs,
  ...
}: {
  nixpkgs.overlays = [
    (_final: prev: {
      grub2 = prev.grub2.overrideAttrs (old: {
        patches =
          (old.patches or [])
          ++ [
            ./grub-cryptodisk-prompt.patch
          ];
      });
    })
  ];
}
