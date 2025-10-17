{ config, pkgs, lib, rcmd, ... }:

let
  dotfiles = ../.;
  pass-otp = pkgs.pass.withExtensions (e: [ e.pass-otp ]);
in {
  home.username = "user";
  home.homeDirectory = "/home/user";
  home.stateVersion = "25.05";
  home.enableNixpkgsReleaseCheck = false;

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
    neovim
    (pkgs.writeShellScriptBin "vi" ''
      exec nvim "$@"
    '')
    (pkgs.writeShellScriptBin "vim" ''
      exec nvim "$@"
    '')

    # email
    mu
    isync
    msmtp

    evince

    # browsers
    firefox

    home-manager
    networkmanagerapplet
    gnupg
    pass-otp
    pinentry-tty
    openssh
    nodejs_22
    yarn
    python3
    meld
    fd
    gcc
    gcc.man
    (pkgs.nodePackages.typescript-language-server)
    (pkgs.nodePackages.typescript)
    lua-language-server

    # árbol base y parsers necesarios
    tree-sitter
    #tree-sitter-langs
    #treesit-grammars.with-all-grammars

    # command line tools
    bat
    tig
    zoxide
    tldr
    eza
    fzf
    ripgrep
    xclip
    wipe
    fdupes
    unzip
    udisks2
    smartmontools

    # editors
    ed

    html2text

    scalpel

    gobuster

    xorriso

    binwalk

    # image viewers
    eog

    # networking tools
    wget
    tcpdump
    ethtool
    wavemon
    nmap
    dig

    openfortivpn

    w3m

    guile

    qemu

    # eca tools
    kicad

    # terminal tools
    picocom
    minicom
    lrzsz
    srecord

    arduino

    dosfstools

    zathura
    testdisk

    # presentation generation
    inkscape
    dia

    cryptsetup

    # 3d printing
    openscad
    prusa-slicer

    # virtualisation
    virt-manager
    virt-viewer

    # utils
    screenkey
    file

    # databases
    dbeaver-bin
    mysql-client

    # rust dev
    cargo

    # personal commands
    rcmd.packages.${pkgs.system}.default

    # java
    pkgs.jdk21
    jdt-language-server

    # ofimatica
    libreoffice

    # man pages
    man-pages
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
    LD_LIBRARY_PATH = "$HOME/.oracle/instantclient_23_8";
  };

  # Incluye el archivo real de i3 desde tu estructura actual
  home.file.".config/i3/config".source = "${dotfiles}/i3/.config/i3/config";

  # i3status config
  home.file.".config/i3status/config".source = "${dotfiles}/i3status/.config/i3status/config";

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
 
  # Configuración de GnuPG
  programs.gpg = {
    enable = true;
  };

  # Configuración del agente de GnuPG
  services.gpg-agent = {
    enable = true;
    pinentry.package = pkgs.pinentry.tty;
    #pinentryFlavor = "tty";  # "gtk2", "qt", "curses", etc.
    #defaultCacheTtl = 1800;
    #maxCacheTtl = 7200;
    enableSshSupport = true;
  };
 
  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;

  programs.msmtp.enable = true;

  programs.emacs = {
    enable = true;
    extraPackages = epkgs: [
      epkgs.mu4e
    ];
  };

  programs.zsh = {
    enable = true;
    oh-my-zsh = {
      enable = true;
      theme = "bira";
      plugins = [ "git" "direnv" "sudo" ];
    };

    enableCompletion = true;
    autosuggestion.enable = true;
    syntaxHighlighting.enable = true;

    shellAliases = {
      rgrep = "grep -r";
    };
    initContent = ''
      eval "$(dircolors ~/.dir_colors)"

      # Asegurar que ~/bin está en el PATH incluso en shells interactivos no-login
      if [[ ":$PATH:" != *":$HOME/bin:"* ]]; then
        export PATH="$HOME/bin:$PATH"
      fi
    '';
  };

  home.file.".dir_colors".source = "${dotfiles}/dir_colors/.dir_colors";

  home.file.".config/mu/mu.cfg".text = ''
    [mu]
    maildir=~/.mail
  '';

  # emacs
  home.file.".emacs.d/init.el".source = "${dotfiles}/emacs/.emacs.d/init.el";
  home.file.".emacs.d/per-system-settings.el".source = "${dotfiles}/emacs/.emacs.d/per-system-settings.el";
  home.file.".emacs.d/modules/ldd-straight.el".source = "${dotfiles}/emacs/.emacs.d/modules/ldd-straight.el";
  home.file.".emacs.d/modules/ldd-settings.el".source = "${dotfiles}/emacs/.emacs.d/modules/ldd-settings.el";
  home.file.".emacs.d/modules/ldd-keys-evil.el".source = "${dotfiles}/emacs/.emacs.d/modules/ldd-keys-evil.el";
  home.file.".emacs.d/modules/ldd-core.el".source = "${dotfiles}/emacs/.emacs.d/modules/ldd-core.el";
  home.file.".emacs.d/modules/ldd-org-roam.el".source = "${dotfiles}/emacs/.emacs.d/modules/ldd-org-roam.el";
  home.file.".emacs.d/modules/ldd-pdf-tools.el".source = "${dotfiles}/emacs/.emacs.d/modules/ldd-pdf-tools.el";
  home.file.".emacs.d/modules/ldd-vertico.el".source = "${dotfiles}/emacs/.emacs.d/modules/ldd-vertico.el";
  home.file.".emacs.d/modules/ldd-android.el".source = "${dotfiles}/emacs/.emacs.d/modules/ldd-android.el";
  home.file.".emacs.d/modules/ldd-scheme.el".source = "${dotfiles}/emacs/.emacs.d/modules/ldd-scheme.el";
  #home.file.".emacs.d/modules/ldd-lua.el".source = "${dotfiles}/emacs/.emacs.d/modules/ldd-lua.el";
  home.file.".emacs.d/modules/ldd-typescript.el".source = "${dotfiles}/emacs/.emacs.d/modules/ldd-typescript.el";
  home.file.".emacs.d/modules/ldd-gpt.el".source = "${dotfiles}/emacs/.emacs.d/modules/ldd-gpt.el";
  home.file.".emacs.d/modules/ldd-mail.el".source = "${dotfiles}/emacs/.emacs.d/modules/ldd-mail.el";

  # nvim
  home.file.".config/nvim/init.lua".source = "${dotfiles}/nvim/.config/nvim/init.lua";
  home.file.".config/nvim/lua".source = "${dotfiles}/nvim/.config/nvim/lua";
  home.file.".config/nvim/ftplugin".source = "${dotfiles}/nvim/.config/nvim/ftplugin";

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
  '';

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

  programs.git = {
    enable = true;

    extraConfig = {
      includeIf."gitdir:/home/user/work/thelabtech/**".path = "/home/user/.config/git/gitconfig-thelabtech";
      includeIf."gitdir:/home/user/courses/aus/**".path = "/home/user/.config/git/gitconfig-aus";
      includeIf."gitdir:/home/user/dev/**".path = "/home/user/.config/git/gitconfig-personal";
      includeIf."gitdir:/home/user/work/projects/**".path = "/home/user/.config/git/gitconfig-personal";
      includeIf."gitdir:/home/user/.dotfiles/".path = "/home/user/.config/git/gitconfig-personal";
    };
  };

  home.file."/home/user/.config/git/gitconfig-thelabtech".text = ''
    [user]
      name = Luciano Daniel Diamand
      email = luciano.diamand@thelabtech.com.ar
    [color]
      ui = always
  '';

  home.file."/home/user/.config/git/gitconfig-aus".text = ''
    [user]
      name = Luciano Daniel Diamand
      email = lucianodiamand@gmail.com
  '';

  home.file."/home/user/.config/git/gitconfig-personal".text = ''
    [user]
      name = Luciano Daniel Diamand
      email = lucianodiamand@gmail.com
  '';

  home.file.".msmtprc" = {
    text = ''
      # Account for Personal Gmail
      account personal-gmail
      host smtp.gmail.com
      port 465
      from lucianodiamand@gmail.com
      auth on
      user lucianodiamand@gmail.com
      passwordeval "pass emails/gmail_mu4e"
      tls on
      tls_starttls off
      logfile ~/.msmtp.log

      # Account for Ips
      account ips
      host smtp.gmail.com
      port 465
      from ldiamand@ips.edu.ar
      auth on
      user ldiamand@ips.edu.ar
      passwordeval "pass emails/ips_mu4e"
      tls on
      tls_starttls off
      logfile ~/.msmtp.log

      # Account for thelabtech
      account thelabtech
      host smtp.gmail.com
      port 465
      from luciano.diamand@thelabtech.com.ar
      auth on
      user luciano.diamand@thelabtech.com.ar
      passwordeval "pass emails/thelab_mu4e"
      tls on
      tls_starttls off
      logfile ~/.msmtp.log

      # Account for fceia
      account fceia
      host smtp-doc.fceia.unr.edu.ar
      port 465
      from ldiamand@fceia.unr.edu.ar
      auth plain
      user ldiamand
      passwordeval "pass emails/fceia"
      tls on
      # Enable STARTTLS if port 587 is used; set to "off" if port 465 is used
      tls_starttls off
      logfile ~/.msmtp.log

      # Account for utn
      account frro
      host mail.frro.utn.edu.ar
      port 587
      from ldiamand@frro.utn.edu.ar
      auth on
      user ldiamand
      passwordeval "pass emails/utn"
      tls on
      # Enable STARTTLS if port 587 is used; set to "off" if port 465 is used
      tls_starttls on
      logfile ~/.msmtp.log

      # Account for yahoo
      account yahoo
      host smtp.mail.yahoo.com
      port 587
      from lucianodiamand@yahoo.com
      auth on
      user lucianodiamand@yahoo.com
      passwordeval "pass emails/yahoo_mu4e"
      tls on
      # Enable STARTTLS if port 587 is used; set to "off" if port 465 is used
      tls_starttls on
      logfile ~/.msmtp.log

      # Default account
      account default : frro
    '';
  };

  # Deploy the isync configuration file.
  home.file.".mbsyncrc" = {
    text = ''
      # General settings
      IMAPAccount personal-gmail
      Host imap.gmail.com
      User lucianodiamand@gmail.com
      PassCmd "pass emails/gmail_mu4e"
      TLSType IMAPS
      AuthMechs LOGIN

      IMAPAccount ips
      Host imap.gmail.com
      User ldiamand@ips.edu.ar
      PassCmd "pass emails/ips_mu4e"
      TLSType IMAPS
      AuthMechs LOGIN

      IMAPAccount thelabtech
      Host imap.gmail.com
      User luciano.diamand@thelabtech.com.ar
      PassCmd "pass emails/thelab_mu4e"
      TLSType IMAPS
      AuthMechs LOGIN

      IMAPAccount yahoo
      Host imap.mail.yahoo.com
      User lucianodiamand@yahoo.com
      PassCmd "pass emails/yahoo_mu4e"
      TLSType IMAPS
      PipelineDepth 1

      IMAPAccount frro
      Host mail.frro.utn.edu.ar
      User ldiamand
      PassCmd "pass emails/utn"
      TLSType STARTTLS

      IMAPAccount fceia
      Host pop-doc.fceia.unr.edu.ar
      User ldiamand
      PassCmd "pass emails/fceia"
      TLSType STARTTLS

      # Local maildir storage
      MaildirStore personal-gmail-local
      Path ~/.mail/personal-gmail/
      Inbox ~/.mail/personal-gmail/Inbox
      Subfolders Verbatim

      MaildirStore ips-local
      Path ~/.mail/ips/
      Inbox ~/.mail/ips/Inbox
      Subfolders Verbatim

      MaildirStore thelabtech-local
      Path ~/.mail/thelabtech/
      Inbox ~/.mail/thelabtech/Inbox
      Subfolders Verbatim

      MaildirStore yahoo-local
      Path ~/.mail/yahoo/
      Inbox ~/.mail/yahoo/Inbox
      SubFolders Verbatim

      MaildirStore frro-local
      Path ~/.mail/frro/
      Inbox ~/.mail/frro/Inbox
      SubFolders Verbatim

      MaildirStore fceia-local
      Path ~/.mail/fceia/
      Inbox ~/.mail/fceia/Inbox
      SubFolders Verbatim

      # Remote storage (IMAP)
      IMAPStore personal-gmail-remote
      Account personal-gmail

      IMAPStore ips-remote
      Account ips

      IMAPStore thelabtech-remote
      Account thelabtech

      IMAPStore yahoo-remote
      Account yahoo

      IMAPStore frro-remote
      Account frro

      IMAPStore fceia-remote
      Account fceia

      # Channels (sync rules)
      Channel personal-gmail
      Far :personal-gmail-remote:
      Near :personal-gmail-local:
      Patterns "INBOX" "[Gmail]/Borradores" "[Gmail]/Enviados" "[Gmail]/Papelera" "[Gmail]/Todos"
      Create Near
      Sync Pull

      Channel ips
      Far :ips-remote:
      Near :ips-local:
      Patterns "INBOX" "[Gmail]/Borradores" "[Gmail]/Enviados" "[Gmail]/Papelera" "[Gmail]/Todos"
      Create Near
      Sync Pull

      Channel thelabtech
      Far :thelabtech-remote:
      Near :thelabtech-local:
      Patterns "INBOX" "Sent" "Drafts" "Trash" "Archive"
      Create Near
      Sync Pull

      Channel yahoo
      Far :yahoo-remote:
      Near :yahoo-local:
      Patterns *
      Create Both
      Sync Pull

      Channel frro
      Far :frro-remote:
      Near :frro-local:
      Patterns *
      Create Both
      Sync All

      Channel fceia
      Far :fceia-remote:
      Near :fceia-local:
      Patterns *
      Create Both
      Sync All

      # Group for syncing all accounts at once
      Group gmail
      Channel personal-gmail
      Channel ips
      Channel thelabtech
      Channel yahoo
      Channel frro
      Channel fceia
    '';
  };

  xdg.configFile."libvirt/libvirt.conf".text = ''
    uri_default = "qemu:///system"
  '';
}

