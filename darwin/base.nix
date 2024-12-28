{
  self,
  config,
  pkgs,
  lib,
  nixpkgs,
  inputs,
  ...
}: {
  # import home-manager and the NUR's base nixos modules
  imports = [
    inputs.home-manager.darwinModules.home-manager
    # FIXME: setup nur for darwin
    # inputs.nur.nixosModules.nur
  ];

  # Pending https://github.com/NixOS/nixpkgs/issues/55674
  options.allowedUnfree = lib.mkOption {
    type = lib.types.listOf lib.types.str;
    default = [];
  };

  config = {
    nix.settings = {
      # enable experimental flake support, since this system config is flake
      # if these options were removed the resultant system would be unable to
      # build a new version of itself (without special intervention)
      experimental-features = ["nix-command" "flakes"];
      # $HOME dotfile clutter is one of my personal pet-peeves, this helps reduce
      # it by storing nix's files in xdg compliant locations
      use-xdg-base-directories = true;
      # this disallows use of nix by non-admin user, since `nix shell` can
      # download and execute arbitrary programs, this helps reduce system attack
      # surface area
      #allowed-users = lib.mkForce ["@wheel"];
    };

    nixpkgs = {
      # this makes NUR packages available under pkgs.nur.repos.repoName.packageName
      overlays = [inputs.nur.overlays.default];

      # Pending https://github.com/NixOS/nixpkgs/issues/55674
      config.allowUnfreePredicate = pkg:
        builtins.elem (lib.getName pkg) (config.allowedUnfree
          ++ (builtins.foldl' (acc: user: acc ++ user.allowedUnfree) []
            (lib.attrsets.attrValues config.home-manager.users)));
    };

    # this disallows use of the `sudo` command by any non-admin user, given
    # sudo's power and historical exploits, this helps reduue system attack
    # surface area
    #security.sudo.execWheelOnly = true;

    # these are packages that i consider absolutely essential for a basic system
    # some of these are default file and network tools you'd find installed by
    # default on most pre-made distros, while others are more exotic and have
    # explanatory comments
    environment.systemPackages = with pkgs; [
      curl
      fd
      file
      git # since this config is in git, git is a absolute necessity
      git-crypt # transparent encryption of files in git on push, used by this config for secrets like password hashes
      lsof
      micro # a fancier version of the nano editor, with a save-with-sudo feature
      nix-output-monitor
      nix-tree # helper tool for interactive inspection of derivations
      pciutils
      unixtools.xxd
      unzip
      wget
      zip
    ];

    # contrary to popular belief, the goal of nix is not to make things more
    # difficult, this helper utility can streamline much of the process of
    # administration a nix system and I recommend it
    #programs.nh.enable = true;

    # FIXME: implicit dependency on neovim.nix
    environment.variables.EDITOR = "nvim";

    # this is boilerplate config to allow home-manager and nixos to interface a
    # little more elegantly
    home-manager = {
      useGlobalPkgs = true;
      useUserPackages = true;
      extraSpecialArgs = {inherit inputs;};
    };

    # Set Git commit hash for darwin-version.
    # system.configurationRevision = self.rev or self.dirtyRev or null;

    # The platform the configuration will be used on.
    nixpkgs.hostPlatform = "x86_64-darwin";

    # Used for backwards compatibility, please read the changelog before changing.
    # $ darwin-rebuild changelog
    system.stateVersion = 5; # Did you read the comment?
  };
}
