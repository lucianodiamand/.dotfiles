{ config, pkgs, lib, rcmd, ... }:

{
  imports = [
    ./home/base.nix
    ./home/ui.nix
    ./home/editors.nix
    ./home/browsers.nix
    ./home/email.nix
    ./home/shell.nix
    ./home/security.nix
    ./home/git.nix
    ./home/cli.nix
    ./home/dev.nix
    ./home/virtualization.nix
    ./home/office.nix
    ./home/misc.nix
  ];
}
