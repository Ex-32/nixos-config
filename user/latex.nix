{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  home.packages = with pkgs; [
    texlive.combined.scheme-full
    texlab
    zathura
  ];

  home.file.".config/latexmk/latexmkrc".text = ''
    $pdf_previewer = 'zathura';
    $latexmk = 'latexmk -interaction=nonstopmode';
  '';
}
