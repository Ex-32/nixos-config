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
        empty-epsilon
        endless-sky
        mindustry
        prismlauncher
        sauerbraten
        superTuxKart
      ];
    }
    (lib.mkIf config.local.lutris.enable {
      home.packages = let
        lutris = pkgs.lutris.override {
          extraLibraries = pkgs: [];
          extraPkgs = pkgs: [];
          steamSupport = false;
        };
      in [lutris];
    })
  ];
}
