{ pkgs, ... }:

let
  dotfiles = ../.;
in {
  home.packages = with pkgs; [
    i3
    i3status
    xss-lock
  ];

  # Incluye el archivo real de i3 desde tu estructura actual
  home.file.".config/i3/config".source = "${dotfiles}/i3/.config/i3/config";

  # i3status config
  home.file.".config/i3status/config".source = "${dotfiles}/i3status/.config/i3status/config";

  home.file.".xinitrc".text = ''
    xrdb -merge ~/.Xresources
    exec i3
  '';
}
