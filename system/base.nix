{ config, pkgs, lib, nixpkgs, inputs, ... }:

{
  # don't get me wrong...   i prefer to use FOSS software, but what i prefer
  # even more is that my system actually fucking works, as such, i consider
  # this a "concession to practicality"
  # TODO: switch to explicit package enabling using allowUnfreePredicate
  nixpkgs.config.allowUnfree = true;

  imports = [
    inputs.home-manager.nixosModules.home-manager
    inputs.nur.nixosModules.nur
  ];

  nix.settings = {
    # enable experimental flake support, since this system config is flake
    # if these options were removed the resultant system would be unable to
    # build a new version of itself (without special intervention)
    experimental-features = [ "nix-command" "flakes" ];
    # $HOME dotfile clutter is one of my personal pet-peeves, this helps reduce
    # it by storing nix's files in xdg compliant locations
    use-xdg-base-directories = true;
    # this disallows use of nix by non-admin user, since `nix shell` can
    # download and execute arbitrary programs, this helps reduce system attack
    # surface area
    allowed-users = lib.mkForce [ "@wheel" ];
  };

  nixpkgs.overlays = [ inputs.nur.overlay ];

  # this stops systemd from keeping coredump images of every crashed program,
  # firstly because doing so presents a security risk (the program could've
  # had sensitive info in memory when it crashed) and secondly because they
  # take up an obscene amount of space
  systemd.coredump.extraConfig = "Storage=none";

  # this disallows use of the `sudo` command by any non-admin user, given
  # sudo's power and historical exploits, this reduces system attack surface
  # area
  security.sudo.execWheelOnly = true;

  # these are packages that i consider absolutely essential for a basic system
  # some of these are default file and network tools you'd find installed by
  # default on most pre-made distros, while others are more exotic and have
  # explanatory comments
  environment.systemPackages = with pkgs; [
    btdu      # a btrfs specific version of ncdu that handles deduplication and transparent compression correctly
    compsize  # tool for checking btrfs transparent compression and disk usage
    curl
    file
    git       # since this config is in git, git is a absolute necessity
    git-crypt # transparent encryption of files in git on push, used by this config for secrets like password hashes
    lsof
    man-pages # core linux man pages aren't in the default package set
    micro     # a fancier version of the nano editor, with a save-with-sudo feature
    unzip
    wget
    zip
  ];

  # this is boilerplate config to allow home-manager and nixos to interface a
  # little more elegantly, we also pass `inputs` as an argument to home-manager
  # modules, so that we can access home-manager specific flake inputs like
  # spicetify-nix
  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    extraSpecialArgs = { inherit inputs; };
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.11"; # Did you read the comment?
}
