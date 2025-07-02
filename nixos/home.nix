{ config, pkgs, ... }:

let
  # Referencia relativa al directorio `.dotfiles`, uno arriba del actual
  dotfiles = ../.;
in {
  home.username = "user";
  home.homeDirectory = "/home/user";
  home.stateVersion = "25.05";

  # Incluye el archivo real de i3 desde tu estructura actual
  home.file.".config/i3/config".source = "${dotfiles}/i3/.config/i3/config";
}

