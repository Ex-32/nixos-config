{ config, pkgs, lib, nixpkgs, ... }:

{
  # enabled docker in rootless mode; traditionally docker runs as a
  # root-privileged daemon, the major drawback of this is that any form of
  # container management required root access, while this can be circumvented
  # by use of the "docker" group, giving user's direct access to the docker
  # daemon is comparable to root acess with the added ability to bypass ACLs
  # and kernel auditing, so yeah...   i'm not doing that
  virtualisation.docker.rootless = {
    enable = true;
    setSocketVariable = true;
  };

  environment.sessionVariables.DOCKER_HOME = "$XDG_CONFIG_HOME/docker";
}
