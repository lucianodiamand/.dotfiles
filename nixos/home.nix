{ config, pkgs, lib, ... }:

let
  dotfiles = ../.;
in {
  home.username = "user";
  home.homeDirectory = "/home/user";
  home.stateVersion = "25.05";
  home.enableNixpkgsReleaseCheck = false;

  home.packages = with pkgs; [
    i3
    i3status
    rxvt-unicode-unwrapped
    dmenu
    pkgs.nerd-fonts.hack
    neovim
    mu
    evince
    firefox
    home-manager
    networkmanagerapplet
    pass
    openssh
    nodejs_20
    yarn
    python3
    ripgrep
    fzf
    unzip
    fd
    gcc
    (pkgs.nodePackages.typescript-language-server)
    (pkgs.nodePackages.typescript)
  ];

  fonts.fontconfig.enable = true;

  # Incluye el archivo real de i3 desde tu estructura actual
  home.file.".config/i3/config".source = "${dotfiles}/i3/.config/i3/config";

  # i3status config
  home.file.".config/i3status/config".source = "${dotfiles}/i3status/.config/i3status/config";

  # .Xresources para rvxt (urxvt)
  home.file.".Xresources".source = "${dotfiles}/rxvt/.Xresources";

  home.file.".xinitrc".text = ''
    xrdb -merge ~/.Xresources
    exec i3
  '';

  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;

  programs.emacs = {
    enable = true;
    extraConfig = ''
      (add-to-list 'load-path "${pkgs.mu}/share/emacs/site-lisp/mu4e")

      ;; Intentar cargar mu4e solo si está disponible en runtime
      (ignore-errors
        (require 'mu4e))
    '';
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
      ll = "ls -l";
    };
  };

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
  home.file.".emacs.d/modules/ldd-typescript.el".source = "${dotfiles}/emacs/.emacs.d/modules/ldd-typescript.el";
  home.file.".emacs.d/modules/ldd-gpt.el".source = "${dotfiles}/emacs/.emacs.d/modules/ldd-gpt.el";
  home.file.".emacs.d/modules/ldd-mail.el".source = "${dotfiles}/emacs/.emacs.d/modules/ldd-mail.el";

  # nvim
  home.file.".config/nvim".source = "${dotfiles}/nvim/.config/nvim";

  home.activation.generateHostingerKey = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    KEYFILE="$HOME/.ssh/bitbucket.l2.repo.key"
    if [ ! -f "$KEYFILE" ]; then
      echo "Generating SSH key for Bitbucket L2 repo..."
      mkdir -p "$HOME/.ssh"
      chmod 700 "$HOME/.ssh"
      ${pkgs.openssh}/bin/ssh-keygen -t ed25519 -f "$KEYFILE" -C "Bitbucket L2 repo" -N ""
      chmod 600 "$KEYFILE"
    fi
  '';

  home.file.".ssh/config".text = ''
    Host bitbucket-l2
      HostName bitbucket.org
      IdentityFile ~/.ssh/bitbucket.l2.repo.key
      IdentitiesOnly yes
  '';

  programs.git = {
    enable = true;

    extraConfig = {
      includeIf."gitdir:/home/user/work/thelabtech/**".path = "/home/user/.config/git/gitconfig-thelabtech";
      includeIf."gitdir:/home/user/courses/aus/**".path = "/home/user/.config/git/gitconfig-aus";
      includeIf."gitdir:/home/user/dev/projects/**".path = "/home/user/.config/git/gitconfig-personal";
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
      email = ldiamand@ips.edu.ar
  '';

  home.file."/home/user/.config/git/gitconfig-personal".text = ''
    [user]
      name = Luciano Daniel Diamand
      email = lucianodiamand@gmail.com
  '';

}

