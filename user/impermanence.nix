{ config, pkgs, lib, inputs, ... }:

{
  home.persistence."/persist/home/jenna" = {
    allowOther = false;
    directories = [
      "documents"
      "src"
      ".mozilla"
      ".ssh"
      ".local/share/gnupg"
      ".config/nvim"
    ];
    files = [

    ];
  };
}
