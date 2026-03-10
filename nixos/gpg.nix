{ pkgs, ... }:

{
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
}
