{
  config,
  pkgs,
  lib,
  nixpkgs,
  ...
}: {
  allowedUnfree = [
    "zerotierone"
  ];

  users.users.puzzle = {
    isNormalUser = true;
    uid = 1001;
    shell = pkgs.zsh;
    extraGroups = ["nix"];
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQC+7JOWyvkjmMJapaYwmXB8YGy6NrfaU+jadmp2fpSkP43d33h/jzGjfEB2sf4rdtl1VWTdL6WO9z3DtbrSkWV/PmUPydb6TvBYX0+NW2dNBgCzc2EJMYR1RHpdqfxXTqJ6beZVHsbYdhiTJPoUJxi9P0NLZVgICy+cXSKcqBrG/EtlQfUYPV1QW0IhqAWUAN4aiXovDBIMsWK+b1ip3bEdX/WFhvamI/c8vI4j5btVpFYj3mUQEDWfNxo0gLJkuT+1nbvOf7CsTf3GthHjbkXPAaSOPdbqfHw4CUnj+9LTUP3zciWoPwU7MHK1I322WSDcTN2kFnL8kip9eN5Pwjzq7HuCR5OjVBD/eyTIh93p/LfXPAIl2jGgiDvsILWEwcZ04TrwstwL6HrGy7kNSrXcjj9oAkCkKhZhgiYcJt7Z1kPUIR2b8IPnxIT7OaMjvnkh7vy5XoRLI9FamFgw5JhWKCvldoGdB2ElF6uhUMHQbGouB2WcyFTJI/hP2E4HN7U= puzzle@ramiel"
    ];
  };

  environment.persistence."/persist/safe/system".directories = [
    "/home/puzzle"
    "/var/lib/zerotier-one"
  ];

  services.zerotierone = {
    enable = true;
    joinNetworks = [
      "8286ac0e474c397c"
    ];
  };
}
