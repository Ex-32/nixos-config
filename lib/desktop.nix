{
  config,
  pkgs,
  lib,
  nixpkgs,
  ...
}: {
  allowedUnfree = [
    "corefonts"
    "xkcd-font"
  ];

  # i hope you can figure this one out
  hardware.graphics.enable = true;

  # this installs a default set of fonts (opensans and the like) so that
  # there's a decent serif, sans-serif, and monospace font installed for
  # well... everything
  fonts.enableDefaultPackages = true;
  fonts.packages = with pkgs; [
    corefonts
    # (google-fonts.override {
    #   fonts = [];
    # })
    noto-fonts
    noto-fonts-cjk-sans
    roboto
    source-sans
    ubuntu_font_family
    xkcd-font
  ];

  # many gtk applications don't behave properly unless they can query dconf for
  # settings, so regardless of windowing environment it's good to have it
  # enabled
  programs.dconf.enable = true;
  environment.systemPackages = [pkgs.adwaita-icon-theme];

  # unprivileged access to disks is a pain in the ass without udisks so, so
  # it's always worth having in a graphical system where you'll be logged in as
  # an unprivileged user most of the time
  services.udisks2.enable = true;

  # some applications expect a dbus provider of the xdg secrets spec, there are
  # a few implementations with gnome-keyring, kwallet, and keepassxc being the
  # most common, keepassxc doesn't daemonize very well and kwallet doesn't work
  # super reliably outside of a full KDE Plasma environment, so we're using
  # gnome-keyring, this could cause issues with trying to install KDE Plasma
  # since it'll likely pull in kwallet which will cause a collision
  services.gnome.gnome-keyring.enable = true;
}
