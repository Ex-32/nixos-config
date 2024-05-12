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
  };

  # set the correct scroll direction
  services.libinput = {
    touchpad.naturalScrolling = true;
    mouse.naturalScrolling = true;
  };
}
