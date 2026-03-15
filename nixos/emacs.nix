{ pkgs, ... }:

let
  dotfiles = ../.;
in {
  programs.emacs = {
    enable = true;
    extraPackages = epkgs: [
      epkgs.mu4e
    ];
  };

  home.file.".emacs.d/init.el".source = "${dotfiles}/emacs/.emacs.d/init.el";
  home.file.".emacs.d/per-system-settings.el".source = "${dotfiles}/emacs/.emacs.d/per-system-settings.el";
  home.file.".emacs.d/modules/ldd-straight.el".source = "${dotfiles}/emacs/.emacs.d/modules/ldd-straight.el";
  home.file.".emacs.d/modules/ldd-settings.el".source = "${dotfiles}/emacs/.emacs.d/modules/ldd-settings.el";
  home.file.".emacs.d/modules/ldd-keys-evil.el".source = "${dotfiles}/emacs/.emacs.d/modules/ldd-keys-evil.el";
  home.file.".emacs.d/modules/ldd-core.el".source = "${dotfiles}/emacs/.emacs.d/modules/ldd-core.el";
  home.file.".emacs.d/modules/ldd-org.el".source = "${dotfiles}/emacs/.emacs.d/modules/ldd-org.el";
  home.file.".emacs.d/modules/ldd-org-roam.el".source = "${dotfiles}/emacs/.emacs.d/modules/ldd-org-roam.el";
  home.file.".emacs.d/modules/ldd-pdf-tools.el".source = "${dotfiles}/emacs/.emacs.d/modules/ldd-pdf-tools.el";
  home.file.".emacs.d/modules/ldd-vertico.el".source = "${dotfiles}/emacs/.emacs.d/modules/ldd-vertico.el";
  home.file.".emacs.d/modules/ldd-android.el".source = "${dotfiles}/emacs/.emacs.d/modules/ldd-android.el";
  home.file.".emacs.d/modules/ldd-scheme.el".source = "${dotfiles}/emacs/.emacs.d/modules/ldd-scheme.el";
  #home.file.".emacs.d/modules/ldd-lua.el".source = "${dotfiles}/emacs/.emacs.d/modules/ldd-lua.el";
  home.file.".emacs.d/modules/ldd-angular.el".source = "${dotfiles}/emacs/.emacs.d/modules/ldd-angular.el";
  home.file.".emacs.d/modules/ldd-typescript.el".source = "${dotfiles}/emacs/.emacs.d/modules/ldd-typescript.el";
  home.file.".emacs.d/modules/ldd-gpt.el".source = "${dotfiles}/emacs/.emacs.d/modules/ldd-gpt.el";
  home.file.".emacs.d/modules/ldd-mail.el".source = "${dotfiles}/emacs/.emacs.d/modules/ldd-mail.el";
  home.file.".emacs.d/modules/ldd-org-gtd.el".source = "${dotfiles}/emacs/.emacs.d/modules/ldd-org-gtd.el";
  home.file.".emacs.d/modules/ldd-present.el".source = "${dotfiles}/emacs/.emacs.d/modules/ldd-present.el";

  home.file."org/roam/templates".source = "${dotfiles}/emacs/templates";
}
