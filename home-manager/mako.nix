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
    backgroundColor = "#100c00";
    borderColor = "#ff006e";
    borderRadius = 0;
    borderSize = 2;
    defaultTimeout = 5000;
    font = "Source Sans 3 12";
    height = 300;
    progressColor = "#252726";
    textColor = "#fcfcfc";
    width = 300;
    extraConfig =
      # toml
      ''
        [urgency=high]
        border-color=#fe552c

        [urgency=low]
        border-color=#00b300
      '';
  };
}
