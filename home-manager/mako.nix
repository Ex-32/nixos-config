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
    borderColor = "#9c00fc";
    borderRadius = 0;
    borderSize = 3;
    defaultTimeout = 5000;
    font = "Source Sans 3 13";
    height = 400;
    progressColor = "#252726";
    textColor = "#fcfcfc";
    width = 400;
    extraConfig =
      # toml
      ''
        [urgency=high]
        border-color=#ff1212

        [urgency=low]
        border-color=#00d500
      '';
  };
}
