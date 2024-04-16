{
  config,
  pkgs,
  lib,
  nixpkgs,
  ...
}: {
  sound.enable = true;

  # i'm using pipewire-pulse to handle pipewire clients and having both enabled
  # causes weird behavior because they'll fight to connect to pulse clients
  hardware.pulseaudio.enable = lib.mkForce false;

  # using rtkit to run pipewire in realtime mode significantly reduces audio
  # jitter/lag
  security.rtkit.enable = true;

  # one audio stack to rule them all, one audio stack to find them; one audio
  # stack to bring them all and in the darkness bind them
  services.pipewire = {
    enable = true;
    wireplumber.enable = true;
    alsa.enable = true;
    pulse.enable = true;
    jack.enable = true;
  };
}
