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
    (nerdfonts.override { fonts = [ "Hack" ]; })
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

  let
    gitIdentities = import ~/.git-identities.nix;
  in {
    programs.git = {
      enable = true;

      userName = gitIdentities.personal.name;
      userEmail = gitIdentities.personal.email;

      includes = [
        {
          condition = "gitdir:${config.home.homeDirectory}/proyectos/cpt/**";
          path = "${config.home.homeDirectory}/.config/git/config-cpt";
        }
        {
          condition = "gitdir:${config.home.homeDirectory}/opensource/**";
          path = "${config.home.homeDirectory}/.config/git/config-oss";
        }
      ];
    };

    home.file.".config/git/config-cpt".text = ''
      [user]
        name = ${gitIdentities.trabajo.name}
        email = ${gitIdentities.trabajo.email}
    '';

    home.file.".config/git/config-oss".text = ''
      [user]
        name = ${gitIdentities.oss.name}
        email = ${gitIdentities.oss.email}
    '';
  }
}

