{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  home = {
    packages = [pkgs.julia];
    file.".config/julia/startup.jl".text = ''
      hex(x::Integer, pad::Integer=1) = string(x, base=16, pad=pad);
      bin(x::Integer, pad::Integer=1) = string(x, base=2, pad=pad);
    '';
  };
}
