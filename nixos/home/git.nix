{ config, pkgs, lib, ... }:

{
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

  home.file."/home/user/.config/git/gitconfig-thelabtech".source = config.sops.templates.gitconfig_thelabtech.path;

  home.file."/home/user/.config/git/gitconfig-aus".source = config.sops.templates.gitconfig_aus.path;

  home.file."/home/user/.config/git/gitconfig-personal".source = config.sops.templates.gitconfig_personal.path;
}
