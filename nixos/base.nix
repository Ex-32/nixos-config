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
    inputs.nur.modules.nixos.default
  ];

  # Pending https://github.com/NixOS/nixpkgs/issues/55674
  options.allowedUnfree = lib.mkOption {
    type = lib.types.listOf lib.types.str;
    default = [];
  };

  config = {
    # define nix group (users who're allowed to interact with the nix daemon)
    users.groups = {nix = {};};

    nix.settings = {
      # enable experimental flake support, since this system config is flake
      # if these options were removed the resultant system would be unable to
      # build a new version of itself (without special intervention)
      experimental-features = ["nix-command" "flakes"];
      # $HOME dotfile clutter is one of my personal pet-peeves, this helps reduce
      # it by storing nix's files in xdg compliant locations
      use-xdg-base-directories = true;
      # this requires that users are specifically added to the "nix" group to
      # interact with the nix daemon
      allowed-users = lib.mkForce ["@nix"];
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

    # this stops systemd from keeping coredump images of every crashed program,
    # firstly because doing so presents a security risk (the program could've
    # had sensitive info in memory when it crashed) and secondly because they
    # take up an obscene amount of space
    systemd.coredump.extraConfig = "Storage=none";

    # after CVE-2025-32463 i'm giving up on sudo, long live doas
    security = {
      sudo.enable = lib.mkForce false;
      doas = {
        enable = true;
        wheelNeedsPassword = true;
        extraRules = [
          {
            groups = ["wheel"];
            persist = true;
          }
        ];
      };
    };

    # these are packages that i consider absolutely essential for a basic system
    # some of these are default file and network tools you'd find installed by
    # default on most pre-made distros, while others are more exotic and have
    # explanatory comments
    environment.systemPackages = with pkgs; [
      curl
      doas-sudo-shim
      efibootmgr
      fd
      file
      git # since this config is in git, git is a absolute necessity
      hyperfine
      lsof
      man-pages
      nix-output-monitor
      nix-tree # helper tool for interactive inspection of derivations
      pciutils
      sops
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
    system.stateVersion = "25.11"; # Did you read the comment?
  };
}
