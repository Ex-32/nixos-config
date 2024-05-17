{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  services.mako = {
    enable = true;
    anchor = "bottom-right";
    backgroundColor = "#1e1e2e";
    borderColor = "#cba6f7";
    borderRadius = 10;
    borderSize = 2;
    defaultTimeout = 5000;
    font = "Raleway 13";
    height = 400;
    progressColor = "#313244";
    textColor = "#cdd6f4";
    width = 400;
    extraConfig =
      # toml
      ''
        [urgency=high]
        border-color=#f38ba8

        [urgency=low]
        border-color=#a6e3a1
      '';
  };
}
