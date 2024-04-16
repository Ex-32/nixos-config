{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  home.packages = [pkgs.texlive.combined.scheme-full];
  home.file.".config/latexmk/latexmkrc".text = ''
    $pdf_previewer = '${pkgs.zathura}/bin/zathura';
    $latexmk = 'latexmk -interaction=nonstopmode';
  '';
}
