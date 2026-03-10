{ pkgs, ... }:

let
  dotfiles = ../.;
in {
  home.packages = [
    (pkgs.rxvt-unicode.override {
      configure = { availablePlugins, ... }: {
        plugins = with availablePlugins; [ perls font-size ];
      };
    })
  ];

  home.file.".Xresources.d" = {
    source = "${dotfiles}/rxvt/.Xresources.d";  # carpeta en tu repo
    recursive = true;   # gestiona todo el árbol
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
