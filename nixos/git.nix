{ ... }:

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
}
