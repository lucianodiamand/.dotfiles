{ config, pkgs, lib, ... }:

{
  home.packages = with pkgs; [
    mu
    isync
    msmtp
  ];

  programs.msmtp.enable = true;

  home.file.".config/mu/mu.cfg".source = config.sops.templates.mu_cfg.path;

  home.file.".msmtprc" = {
    source = config.sops.templates.msmtprc.path;
  };

  # Deploy the isync configuration file.
  home.file.".mbsyncrc" = {
    source = config.sops.templates.mbsyncrc.path;
  };
}
