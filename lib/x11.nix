{
  config,
  pkgs,
  lib,
  nixpkgs,
  ...
}: {
  services.xserver = {
    enable = true;
    autorun = false;

    # my preferred login method is a tty, so i use startx to start X-based
    # graphical sessions
    displayManager.startx.enable = true;

    xkb.options = "compose:ralt,ctrl:nocaps";
  };

  # set the correct scroll direction
  services.libinput = {
    touchpad = {
      naturalScrolling = true;
      tapping = false;              # disable tap-to-click
      clickMethod = "clickfinger";  # mouse button click based on number of fingers 
    };
    mouse.naturalScrolling = true;
  };
}
