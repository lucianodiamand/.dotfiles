{ config, pkgs, lib, ... }:

let
  dotfiles = ../../.;
in {
  home.packages = with pkgs; [
    i3
    i3status
    xss-lock
    (rxvt-unicode.override {
      configure = { availablePlugins, ... }: {
        plugins = with availablePlugins; [ perls font-size ];
      };
    })
    dmenu
    pkgs.nerd-fonts.hack
    evince
    eog
    zathura
    screenkey
    networkmanagerapplet
  ];

  # Incluye el archivo real de i3 desde tu estructura actual
  home.file.".config/i3/config".source = "${dotfiles}/i3/.config/i3/config";

  # i3status config
  home.file.".config/i3status/config".source = "${dotfiles}/i3status/.config/i3status/config";

  home.file.".Xresources.d" = {
    source = "${dotfiles}/rxvt/.Xresources.d";  # carpeta en tu repo
    recursive = true;   # gestiona todo el arbol
    force = true;       # reemplaza si ya existe
  };

  home.file.".Xresources.base".source = "${dotfiles}/rxvt/.Xresources.base";
  home.file.".Xresources.solarized-dark".source = "${dotfiles}/rxvt/.Xresources.solarized-dark";
  home.file.".Xresources.solarized-light".source = "${dotfiles}/rxvt/.Xresources.solarized-light";

  # .Xresources para rvxt (urxvt)
  home.file.".Xresources".source = "${dotfiles}/rxvt/.Xresources.d/solarized-light";

  home.file.".xinitrc".text = ''
    xrdb -merge ~/.Xresources
    exec i3
  '';
}
