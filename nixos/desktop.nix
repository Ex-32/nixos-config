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
  fonts.packages =
    (with pkgs; [
      corefonts
      (google-fonts.override {
        fonts = [
          "Courier Prime"
          "Permanent Marker"
        ];
      })
      noto-fonts
      noto-fonts-cjk-sans
      roboto
      source-sans
      ubuntu-classic
      xkcd-font
    ])
    ++ (with pkgs.nerd-fonts; [
      _3270
      anonymice
      bigblue-terminal
      departure-mono
      fira-code
      gohufont
      heavy-data
      iosevka-term-slab
      lekton
      monofur
      monoid
      open-dyslexic
      proggy-clean-tt
      recursive-mono
      space-mono
      symbols-only
      terminess-ttf
      victor-mono
    ]);

  # many gtk applications don't behave properly unless they can query dconf for
  # settings, so regardless of windowing environment it's good to have it
  # enabled
  programs.dconf.enable = true;
  environment.systemPackages = with pkgs; [
    adwaita-icon-theme

    # in order for pkexec to do correctly grant gparted permission to modify the
    # disk it needs to be installed at the system level not the user level,
    # realistically i want gparted on any system with a graphical interface, so
    # this is the logical place for it
    gparted
  ];

  # unprivileged access to disks is a pain in the ass without udisks so, so
  # it's always worth having in a graphical system where you'll be logged in as
  # an unprivileged user most of the time
  services.udisks2.enable = true;

  # needed for home-manager xdg portals to work correctly
  environment.pathsToLink = ["/share/xdg-desktop-portal" "/share/applications"];
}
