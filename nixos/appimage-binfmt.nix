{
  config,
  pkgs,
  lib,
  nixpkgs,
  ...
}: {
  # this registers a kernel binfmt for appimage files that runs them with
  # `appimage-run` so that they work correctly on nixos
  boot.binfmt.registrations.appimage = {
    wrapInterpreterInShell = false;
    interpreter = "${pkgs.appimage-run}/bin/appimage-run";
    recognitionType = "magic";
    offset = 0;
    mask = ''\xff\xff\xff\xff\x00\x00\x00\x00\xff\xff\xff'';
    magicOrExtension = ''\x7fELF....AI\x02'';
  };
}
