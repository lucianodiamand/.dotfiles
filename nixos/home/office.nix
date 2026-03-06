{ config, pkgs, lib, ... }:

{
  home.packages = with pkgs; [
    libreoffice
    inkscape
    dia
  ];
}
