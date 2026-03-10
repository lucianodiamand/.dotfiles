{ config, pkgs, lib, rcmd, ... }:

let
  dotfiles = ../.;
in {
  imports = [ ./w3m.nix ./bat.nix ./btop.nix ./zathura.nix ./emacs.nix ./nvim.nix ./ssh.nix ./mail.nix ./git.nix ./rxvt.nix ./i3.nix ./pass.nix ./gpg.nix ./gpt-term.nix ];
  home.username = "user";
  home.homeDirectory = "/home/user";
  home.stateVersion = "25.05";
  home.enableNixpkgsReleaseCheck = false;

  home.packages = with pkgs; [
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

  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;

  programs.zoxide = {
    enable = true;
    enableZshIntegration = true;
  };

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
