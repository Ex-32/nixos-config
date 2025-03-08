{
  config,
  pkgs,
  lib,
  inputs,
  nixpkgs,
  ...
}: {
  imports = [
    inputs.nur.modules.nixos.default
  ];

  systemd.services.ccc = let
    ccc = pkgs.nur.repos.Ex-32.ccc;
    config = ''
      [Files]
      Lock = /tmp/ccc.lock

      [Log]
      Silent = false

      [Options]
      Sync = true

      [Levels] # percents
      First  = 15
      Second = 10
      Third  = 5

      [Timeouts] # seconds
      Check = 10

      [Error]
      MaxAmount = 10
    '';
  in {
    description = "ccc ram cache dropper";
    wantedBy = ["multi-user.target"];
    serviceConfig = {
      ExecStart = "${lib.getExe ccc} -c ${builtins.toFile "ccc.conf" config}";
    };
  };
}
