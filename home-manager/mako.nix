{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  services.mako = {
    enable = true;
    settings = {
      anchor = "bottom-right";
      background-color = "#100c00";
      border-color = "#ff006e";
      border-radius = 0;
      border-size = 4;
      default-timeout = 5000;
      font = "Source Sans 3 12";
      height = 300;
      progress-color = "#252726";
      text-color = "#fcfcfc";
      width = 300;
      "urgency=high".border-color = "#fe552c";
      "urgency=low".border-color = "#00b300";
    };
  };
}
