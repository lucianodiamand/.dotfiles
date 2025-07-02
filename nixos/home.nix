{ config, pkgs, ... }:

let
  # Referencia relativa al directorio `.dotfiles`, uno arriba del actual
  dotfiles = ../.;
in {
  home.username = "user";
  home.homeDirectory = "/home/user";
  home.stateVersion = "25.05";

  home.packages = with pkgs; [
    i3
    i3status
    rxvt_unicode
    dmenu
  ];

  # Incluye el archivo real de i3 desde tu estructura actual
  home.file.".config/i3/config".source = "${dotfiles}/i3/.config/i3/config";

  # i3status config
  home.activation.generateI3StatusConfig = lib.hm.dag.entryBefore [ "writeBoundary" ] ''
    echo "Generando config de i3status..."
    /home/luciano/.dotfiles/i3status/generate.sh #> $HOME/.config/i3status/config
  '';
  #home.file.".config/i3status/config".source = "${dotfiles}/i3status/.config/i3status/config";

  # .Xresources para rvxt (urxvt)
  home.file.".Xresources".source = "${dotfiles}/rvxt/.Xresources";

  # si tenés .Xdefaults
  # home.file.".Xdefaults".source = "${dotfiles}/rvxt/.Xdefaults";
}

