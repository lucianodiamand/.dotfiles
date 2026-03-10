{ pkgs, ... }:

let
  dotfiles = ../.;
in {
  home.packages = [ pkgs.bat ];

  home.file.".config/bat/config".source = "${dotfiles}/bat/.config/bat/config";
}
