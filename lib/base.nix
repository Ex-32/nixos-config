{
  config,
  pkgs,
  lib,
  nixpkgs,
  inputs,
  ...
}: {
  # import home-manager and the NUR's base nixos modules
  imports = [
    inputs.home-manager.nixosModules.home-manager
    inputs.nur.nixosModules.nur
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
      allowed-users = lib.mkForce ["@wheel"];
    };

    nixpkgs = {
      # this makes NUR packages available under pkgs.nur.repos.repoName.packageName
      overlays = [inputs.nur.overlay];

      # Pending https://github.com/NixOS/nixpkgs/issues/55674
      config.allowUnfreePredicate = pkg:
        builtins.elem (lib.getName pkg) (config.allowedUnfree
          ++ (builtins.foldl' (acc: user: acc ++ user.allowedUnfree) []
            (lib.attrsets.attrValues config.home-manager.users)));
    };

    # this stops systemd from keeping coredump images of every crashed program,
    # firstly because doing so presents a security risk (the program could've
    # had sensitive info in memory when it crashed) and secondly because they
    # take up an obscene amount of space
    systemd.coredump.extraConfig = "Storage=none";

    # this disallows use of the `sudo` command by any non-admin user, given
    # sudo's power and historical exploits, this helps reduce system attack
    # surface area
    security.sudo.execWheelOnly = true;

    # these are packages that i consider absolutely essential for a basic system
    # some of these are default file and network tools you'd find installed by
    # default on most pre-made distros, while others are more exotic and have
    # explanatory comments
    environment.systemPackages = with pkgs; [
      curl
      efibootmgr
      fd
      file
      git # since this config is in git, git is a absolute necessity
      git-crypt # transparent encryption of files in git on push, used by this config for secrets like password hashes
      lsof
      man-pages
      micro # a fancier version of the nano editor, with a save-with-sudo feature
      nix-output-monitor
      nix-tree # helper tool for interactive inspection of derivations
      pciutils
      unixtools.xxd
      unzip
      usbutils
      wget
      zip
    ];

    boot.binfmt.emulatedSystems = [
      "aarch64-linux"
      "riscv64-linux"
    ];

    # contrary to popular belief, the goal of nix is not to make things more
    # difficult, this helper utility can streamline much of the process of
    # administration a nix system and I recommend it
    programs.nh.enable = true;

    # FIXME: implicit dependency on neovim.nix
    environment.variables.EDITOR = "nvim";

    # this is boilerplate config to allow home-manager and nixos to interface a
    # little more elegantly
    home-manager = {
      useGlobalPkgs = true;
      useUserPackages = true;
      extraSpecialArgs = {inherit inputs;};
    };

    # This value determines the NixOS release from which the default
    # settings for stateful data, like file locations and database versions
    # on your system were taken. It‘s perfectly fine and recommended to leave
    # this value at the release version of the first install of this system.
    # Before changing this value read the documentation for this option
    # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
    system.stateVersion = "23.11"; # Did you read the comment?
  };
}
