{ config, pkgs, lib, rcmd, ... }:

let
  dotfiles = ../.;
  pass-otp = pkgs.pass.withExtensions (e: [ e.pass-otp ]);
in {
  imports = [ ./w3m.nix ./bat.nix ./btop.nix ./zathura.nix ./emacs.nix ./nvim.nix ./ssh.nix ./mail.nix ./git.nix ./rxvt.nix ];
  home.username = "user";
  home.homeDirectory = "/home/user";
  home.stateVersion = "25.05";
  home.enableNixpkgsReleaseCheck = false;

  home.packages = with pkgs; [
    i3
    i3status
    xss-lock
    dmenu
    pkgs.nerd-fonts.hack
    (pkgs.writeShellScriptBin "vi" ''
      exec nvim "$@"
    '')
    (pkgs.writeShellScriptBin "vim" ''
      exec nvim "$@"
    '')


    evince

    # browsers
    firefox

    home-manager
    networkmanagerapplet
    gnupg
    pass-otp
    #pinentry-tty
    pinentry-gtk2
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
    usbutils
    git-filter-repo

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
    inetutils

    openfortivpn

    buku

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
    jq

    # databases
    dbeaver-bin
    mariadb.client

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

    # programming tools
    universal-ctags
    cscope
    entr

    # forensics
    ghidra-bin

    # multimedia
    mpv
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

  # Incluye el archivo real de i3 desde tu estructura actual
  home.file.".config/i3/config".source = "${dotfiles}/i3/.config/i3/config";

  # i3status config
  home.file.".config/i3status/config".source = "${dotfiles}/i3status/.config/i3status/config";

  # Configuración de GnuPG
  programs.gpg = {
    enable = true;
  };

  # Configuración del agente de GnuPG
  services.gpg-agent = {
    enable = true;
    pinentry.package = pkgs.pinentry.gtk2;
    #pinentryFlavor = "tty";  # "gtk2", "qt", "curses", etc.
    #defaultCacheTtl = 1800;
    #maxCacheTtl = 7200;
    enableSshSupport = true;
  };
 
  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;

  programs.zsh = {
    enable = true;
    autocd = false;
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

      OPENAI_PASS_ENTRY="sites/openai-api-key"

      gpt_term() {
        if ! command -v pass >/dev/null 2>&1; then
          print -u2 "ERROR: falta pass"
          return 1
        fi
        if ! command -v jq >/dev/null 2>&1; then
          print -u2 "ERROR: falta jq"
          return 1
        fi
        if ! command -v curl >/dev/null 2>&1; then
          print -u2 "ERROR: falta curl"
          return 1
        fi

        local entry
        entry="$OPENAI_PASS_ENTRY"
        if [[ -z "$entry" ]]; then
          entry="openai/api-key"
        fi

        local api_key
        api_key="$(pass show "$entry" 2>/dev/null | head -n1)"
        if [[ -z "$api_key" ]]; then
          print -u2 "ERROR: no pude leer la key desde pass ($entry)"
          return 1
        fi

        local prompt
        prompt="$*"
        if [[ -z "$prompt" ]]; then
          print -u2 "Uso: ?? <pregunta>"
          return 1
        fi

        local tmp http_status
        tmp="$(mktemp)"

        http_status="$(curl -sS -o "$tmp" -w "%{http_code}" https://api.openai.com/v1/responses \
          -H "Content-Type: application/json" \
          -H "Authorization: Bearer $api_key" \
          -d "$(jq -n --arg input "$prompt" '{model:"gpt-5.2", input:$input}')" )"

        if [[ "$http_status" != 2* ]]; then
          print -u2 "HTTP $http_status"
          jq -r 'if .error and .error.message then .error.message else . end' < "$tmp" >&2
          rm -f "$tmp"
          return 1
        fi

        jq -r 'if .output_text then .output_text else "SIN output_text. Dump de .output:\n" + (.output|tostring) end' < "$tmp"
        rm -f "$tmp"
      }

      # ahora sí: los comandos exactos
      alias '??'='gpt_term'
    '';
  };

  home.file.".dir_colors".source = "${dotfiles}/dir_colors/.dir_colors";

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
  xdg.configFile."libvirt/libvirt.conf".text = ''
    uri_default = "qemu:///system"
  '';
}
