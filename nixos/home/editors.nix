{ config, pkgs, lib, ... }:

let
  dotfiles = ../../.;
in {
  home.packages = with pkgs; [
    neovim
    (pkgs.writeShellScriptBin "vi" ''
      exec nvim "$@"
    '')
    (pkgs.writeShellScriptBin "vim" ''
      exec nvim "$@"
    '')
    ed
  ];

  programs.emacs = {
    enable = true;
    extraPackages = epkgs: [
      epkgs.mu4e
    ];
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
  #home.file.".emacs.d/modules/ldd-lua.el".source = "${dotfiles}/emacs/.emacs.d/modules/ldd-lua.el";
  home.file.".emacs.d/modules/ldd-angular.el".source = "${dotfiles}/emacs/.emacs.d/modules/ldd-angular.el";
  home.file.".emacs.d/modules/ldd-typescript.el".source = "${dotfiles}/emacs/.emacs.d/modules/ldd-typescript.el";
  home.file.".emacs.d/modules/ldd-gpt.el".source = "${dotfiles}/emacs/.emacs.d/modules/ldd-gpt.el";
  home.file.".emacs.d/modules/ldd-mail.el".source = "${dotfiles}/emacs/.emacs.d/modules/ldd-mail.el";
  home.file.".emacs.d/modules/ldd-org-gtd.el".source = "${dotfiles}/emacs/.emacs.d/modules/ldd-org-gtd.el";
  home.file.".emacs.d/modules/ldd-present.el".source = "${dotfiles}/emacs/.emacs.d/modules/ldd-present.el";

  # nvim
  home.file.".config/nvim/init.lua".source = "${dotfiles}/nvim/.config/nvim/init.lua";
  home.file.".config/nvim/lua".source = "${dotfiles}/nvim/.config/nvim/lua";
  home.file.".config/nvim/ftplugin".source = "${dotfiles}/nvim/.config/nvim/ftplugin";
}
