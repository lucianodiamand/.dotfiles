{ pkgs, ... }:

let
  dotfiles = ../.;
in {
  home.packages = [ pkgs.neovim ];

  home.file.".config/nvim/init.lua".source = "${dotfiles}/nvim/.config/nvim/init.lua";
  home.file.".config/nvim/lua".source = "${dotfiles}/nvim/.config/nvim/lua";
  home.file.".config/nvim/ftplugin".source = "${dotfiles}/nvim/.config/nvim/ftplugin";
}
