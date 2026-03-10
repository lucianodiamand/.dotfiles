{ pkgs, ... }:

let
  dotfiles = ../.;
in {
  home.packages = [ pkgs.zathura ];

  home.file.".config/zathura/zathurarc".source = "${dotfiles}/zathura/.config/zathura/zathurarc";
}
