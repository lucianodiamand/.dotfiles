{ pkgs, ... }:

let
  dotfiles = ../.;
in {
  home.packages = [ pkgs.btop ];

  home.file.".config/btop/btop.conf".source = "${dotfiles}/btop/.config/btop/btop.conf";
}
