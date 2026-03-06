{ config, pkgs, lib, ... }:

let
  pass-otp = pkgs.pass.withExtensions (e: [ e.pass-otp ]);
in {
  home.packages = with pkgs; [
    gnupg
    pass-otp
    pinentry-gtk2
    openssh
    cryptsetup
  ];

  # Configuracion de GnuPG
  programs.gpg = {
    enable = true;
  };

  # Configuracion del agente de GnuPG
  services.gpg-agent = {
    enable = true;
    pinentry.package = pkgs.pinentry.gtk2;
    #pinentryFlavor = "tty";  # "gtk2", "qt", "curses", etc.
    #defaultCacheTtl = 1800;
    #maxCacheTtl = 7200;
    enableSshSupport = true;
  };

  home.activation.generateHostingerKey = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    KEYFILE="$HOME/.ssh/bitbucket.l2.repo.key"
    if [ ! -f "$KEYFILE" ]; then
      echo "Generating SSH key for Bitbucket L2 repo..."
      mkdir -p "$HOME/.ssh"
      chmod 700 "$HOME/.ssh"
      ${pkgs.openssh}/bin/ssh-keygen -t ed25519 -f "$KEYFILE" -C "Bitbucket L2 repo" -N ""
      chmod 600 "$KEYFILE"
    fi

    KEYFILE="$HOME/.ssh/cpt-prod.server.key"
    if [ ! -f "$KEYFILE" ]; then
      echo "Generating SSH key for cpt server..."
      mkdir -p "$HOME/.ssh"
      chmod 700 "$HOME/.ssh"
      ${pkgs.openssh}/bin/ssh-keygen -t ed25519 -f "$KEYFILE" -C "Donweb cpt vps server" -N ""
      chmod 600 "$KEYFILE"
    fi

    KEYFILE="$HOME/.ssh/github.personal.repo.key"
    if [ ! -f "$KEYFILE" ]; then
      echo "Generating SSH key for personal Github repo..."
      mkdir -p "$HOME/.ssh"
      chmod 700 "$HOME/.ssh"
      ${pkgs.openssh}/bin/ssh-keygen -t ed25519 -f "$KEYFILE" -C "Personal Github repo" -N ""
      chmod 600 "$KEYFILE"
    fi

    KEYFILE="$HOME/.ssh/arq-desa.server.key"
    if [ ! -f "$KEYFILE" ]; then
      echo "Generating SSH key for arq desa server..."
      mkdir -p "$HOME/.ssh"
      chmod 700 "$HOME/.ssh"
      ${pkgs.openssh}/bin/ssh-keygen -t ed25519 -f "$KEYFILE" -C "Arq desa server" -N ""
      chmod 600 "$KEYFILE"
    fi
  '';

  home.file.".ssh/config".text = ''
    Host bitbucket-l2
      HostName bitbucket.org
      IdentityFile ~/.ssh/bitbucket.l2.repo.key
      IdentitiesOnly yes

    Host cptprod
      HostName 179.43.117.86
      IdentityFile ~/.ssh/cpt-prod.server.key
      Port 4626

    Host github.com
      IdentityFile ~/.ssh/github.personal.repo.key

    Host arqdesa
      HostName 149.50.150.213
      User root
      IdentityFile ~/.ssh/arq-desa.server.key
      Port 5961

    Host sunde
      HostName 192.168.122.191
      HostKeyAlgorithms +ssh-rsa
      PubkeyAcceptedAlgorithms +ssh-rsa
      SetEnv TERM=rxvt
  '';
}
