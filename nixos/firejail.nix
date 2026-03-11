{ pkgs, ... }:

let
  dotfiles = ../.;
in {
  home.packages = [ pkgs.btop ];

  home.file.".config/firejail/firefox-bna.profile".source = "${dotfiles}/firejail/.config/firejail/firefox-bna.profile";
  home.file.".config/firejail/bna.net".source = "${dotfiles}/firejail/.config/firejail/bna.net";
}
