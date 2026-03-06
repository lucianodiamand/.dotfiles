{ config, pkgs, lib, ... }:

{
  home.packages = with pkgs; [
    openscad
    prusa-slicer
  ];
}
