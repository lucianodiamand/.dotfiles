{ lib, pkgs, ... }:

let
  dotfiles = ../.;
in {
  home.packages = [ pkgs.btop ];

  home.activation.firejailFiles = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    mkdir -p "$HOME/.config/firejail"
    install -m 0644 "${dotfiles}/firejail/.config/firejail/firefox-bna.profile" \
      "$HOME/.config/firejail/firefox-bna.profile"
    install -m 0644 "${dotfiles}/firejail/.config/firejail/bna.net" \
      "$HOME/.config/firejail/bna.net"
  '';
}
