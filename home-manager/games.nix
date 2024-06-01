{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  options.local = {
    lutris.enable = lib.mkEnableOption "Lutris Gaming Platform";
  };

  config = lib.mkMerge [
    {
      home.packages = with pkgs; [
        prismlauncher
        superTuxKart
      ];
    }
    (lib.mkIf config.local.lutris.enable {
      home.packages = let
        lutris = pkgs.lutris.override {
          extraLibraries = pkgs: with pkgs; [];
          extraPkgs = pkgs: with pkgs; [];
          steamSupport = false;
        };
      in [lutris];
    })
  ];
}
