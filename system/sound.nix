{ config, pkgs, lib, nixpkgs, ... }:

{
  sound.enable = true;

  # i'm using pipewire-pulse to handle pipewire clients, but faster
  hardware.pulseaudio.enable = false;

  # using rtkit to run pipewire in real-time mode significantly reduces audio
  # jitter or lag
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
