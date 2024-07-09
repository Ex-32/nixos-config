{
  config,
  pkgs,
  lib,
  nixpkgs,
  ...
}: {
  allowedUnfree = [
    "cnijfilter2"
    "hplip"
  ];

  services.printing = {
    enable = true;
    startWhenNeeded = true;

    # NOTE: printer drivers *must* be put here, just adding them to
    # environment.systemPackages doesn't make them available to to CUPS
    drivers = with pkgs; [
      brlaser
      cnijfilter2
      gutenprint
      gutenprintBin
      (hplipWithPlugin.override {
        # HACK: remove this when fixed upstream
        python3Packages = python311Packages;
      })
    ];
  };

  # this is needed for wifi printing and automatic printer discovery to work
  services.avahi = {
    enable = true;
    nssmdns4 = true;
    nssmdns6 = true;
    openFirewall = true;
  };
}
