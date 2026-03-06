{ config, pkgs, lib, rcmd, ... }:

let
  dotfiles = ../../.;
in {
  home.username = "user";
  home.homeDirectory = "/home/user";
  home.stateVersion = "25.05";
  home.enableNixpkgsReleaseCheck = false;

  home.packages = with pkgs; [
    home-manager
    man-pages
    rcmd.packages.${pkgs.system}.default
  ];

  fonts.fontconfig.enable = true;

  home.file."bin" = {
    source = dotfiles + /zsh/bin;
    recursive = true;
    force = true;
  };
  home.file.".project-templates".source = "${dotfiles}/nixos/project-templates";

  home.sessionPath = [
    "$HOME/bin"
  ];

  home.sessionVariables = {
    EDITOR = "vi";
    VISUAL = "vi";
    INSTANT_CLIENT_PATH = "$HOME/.oracle/instantclient_23_8";
  };

  home.activation.installOracleInstantClientDir = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    if [ ! -d "$HOME/.oracle/instantclient_23_8" ]; then
      echo "Creando directorio y ejecutando script..."

      mkdir -p "$HOME/.oracle"

      INSTANT_CLIENT=instantclient-basic-linux.x64-23.8.0.25.04.zip
      url_base='https://download.oracle.com/otn_software/linux/instantclient/2380000/'

      ${pkgs.wget}/bin/wget -c "$url_base$INSTANT_CLIENT" -P /tmp
      ${pkgs.unzip}/bin/unzip "/tmp/$INSTANT_CLIENT" -d "$HOME/.oracle"
    fi
  '';

  home.activation.createDirs = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    mkdir -p ~/.mail/personal-gmail
    mkdir -p ~/.mail/thelabtech
    mkdir -p ~/.mail/fceia
    mkdir -p ~/.mail/yahoo
    mkdir -p ~/.mail/frro
    mkdir -p ~/.mail/ips

    mkdir -p ~/dev

    mkdir -p ~/courses/aus/taller2
    mkdir -p ~/courses/aus/taller3
    mkdir -p ~/courses/aus/seminario3

    mkdir -p ~/work/thelabtech/l2
    mkdir -p ~/work/personal

    mkdir -p ~/Documents
    mkdir -p ~/Downloads

    # notes: conocimiento estable
    # labs:  experimentos
    # projects: cosas que queremos terminar
    # tasks: tareas independientes
    # meetings: reuniones
    # versions: changelogs / releases
    # dailies: bitacora
    mkdir -p ~/org/roam/{projects,tasks,notes,meetings,versions,dailies,slides,labs}
  '';
  home.file."org/roam/templates".source = "${dotfiles}/emacs/templates";
}
