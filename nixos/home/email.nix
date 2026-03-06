{ config, pkgs, lib, ... }:

{
  home.packages = with pkgs; [
    mu
    isync
    msmtp
  ];

  programs.msmtp.enable = true;

  home.file.".config/mu/mu.cfg".text = ''
    [mu]
    maildir=~/.mail
  '';

  home.file.".msmtprc" = {
    text = ''
      # Account for Personal Gmail
      account personal-gmail
      host smtp.gmail.com
      port 465
      from lucianodiamand@gmail.com
      auth on
      user lucianodiamand@gmail.com
      passwordeval "pass emails/gmail_mu4e"
      tls on
      tls_starttls off
      logfile ~/.msmtp.log

      # Account for Ips
      account ips
      host smtp.gmail.com
      port 465
      from ldiamand@ips.edu.ar
      auth on
      user ldiamand@ips.edu.ar
      passwordeval "pass emails/ips_mu4e"
      tls on
      tls_starttls off
      logfile ~/.msmtp.log

      # Account for thelabtech
      account thelabtech
      host smtp.gmail.com
      port 465
      from luciano.diamand@thelabtech.com.ar
      auth on
      user luciano.diamand@thelabtech.com.ar
      passwordeval "pass emails/thelab_mu4e"
      tls on
      tls_starttls off
      logfile ~/.msmtp.log

      # Account for fceia
      account fceia
      host smtp-doc.fceia.unr.edu.ar
      port 465
      from ldiamand@fceia.unr.edu.ar
      auth plain
      user ldiamand
      passwordeval "pass emails/fceia"
      tls on
      # Enable STARTTLS if port 587 is used; set to "off" if port 465 is used
      tls_starttls off
      logfile ~/.msmtp.log

      # Account for utn
      account frro
      host mail.frro.utn.edu.ar
      port 587
      from ldiamand@frro.utn.edu.ar
      auth on
      user ldiamand
      passwordeval "pass emails/utn"
      tls on
      # Enable STARTTLS if port 587 is used; set to "off" if port 465 is used
      tls_starttls on
      logfile ~/.msmtp.log

      # Account for yahoo
      account yahoo
      host smtp.mail.yahoo.com
      port 587
      from lucianodiamand@yahoo.com
      auth on
      user lucianodiamand@yahoo.com
      passwordeval "pass emails/yahoo_mu4e"
      tls on
      # Enable STARTTLS if port 587 is used; set to "off" if port 465 is used
      tls_starttls on
      logfile ~/.msmtp.log

      # Default account
      account default : frro
    '';
  };

  # Deploy the isync configuration file.
  home.file.".mbsyncrc" = {
    text = ''
      # General settings
      IMAPAccount personal-gmail
      Host imap.gmail.com
      User lucianodiamand@gmail.com
      PassCmd "pass emails/gmail_mu4e"
      TLSType IMAPS
      AuthMechs LOGIN

      IMAPAccount ips
      Host imap.gmail.com
      User ldiamand@ips.edu.ar
      PassCmd "pass emails/ips_mu4e"
      TLSType IMAPS
      AuthMechs LOGIN

      IMAPAccount thelabtech
      Host imap.gmail.com
      User luciano.diamand@thelabtech.com.ar
      PassCmd "pass emails/thelab_mu4e"
      TLSType IMAPS
      AuthMechs LOGIN

      IMAPAccount yahoo
      Host imap.mail.yahoo.com
      User lucianodiamand@yahoo.com
      PassCmd "pass emails/yahoo_mu4e"
      TLSType IMAPS
      PipelineDepth 1

      IMAPAccount frro
      Host mail.frro.utn.edu.ar
      User ldiamand
      PassCmd "pass emails/utn"
      TLSType STARTTLS

      IMAPAccount fceia
      Host pop-doc.fceia.unr.edu.ar
      User ldiamand
      PassCmd "pass emails/fceia"
      TLSType STARTTLS

      # Local maildir storage
      MaildirStore personal-gmail-local
      Path ~/.mail/personal-gmail/
      Inbox ~/.mail/personal-gmail/Inbox
      Subfolders Verbatim

      MaildirStore ips-local
      Path ~/.mail/ips/
      Inbox ~/.mail/ips/Inbox
      Subfolders Verbatim

      MaildirStore thelabtech-local
      Path ~/.mail/thelabtech/
      Inbox ~/.mail/thelabtech/Inbox
      Subfolders Verbatim

      MaildirStore yahoo-local
      Path ~/.mail/yahoo/
      Inbox ~/.mail/yahoo/Inbox
      SubFolders Verbatim

      MaildirStore frro-local
      Path ~/.mail/frro/
      Inbox ~/.mail/frro/Inbox
      SubFolders Verbatim

      MaildirStore fceia-local
      Path ~/.mail/fceia/
      Inbox ~/.mail/fceia/Inbox
      SubFolders Verbatim

      # Remote storage (IMAP)
      IMAPStore personal-gmail-remote
      Account personal-gmail

      IMAPStore ips-remote
      Account ips

      IMAPStore thelabtech-remote
      Account thelabtech

      IMAPStore yahoo-remote
      Account yahoo

      IMAPStore frro-remote
      Account frro

      IMAPStore fceia-remote
      Account fceia

      # Channels (sync rules)
      Channel personal-gmail
      Far :personal-gmail-remote:
      Near :personal-gmail-local:
      Patterns "INBOX" "[Gmail]/Borradores" "[Gmail]/Enviados" "[Gmail]/Papelera" "[Gmail]/Todos"
      Create Near
      Sync Pull

      Channel ips
      Far :ips-remote:
      Near :ips-local:
      Patterns "INBOX" "[Gmail]/Borradores" "[Gmail]/Enviados" "[Gmail]/Papelera" "[Gmail]/Todos"
      Create Near
      Sync Pull

      Channel thelabtech
      Far :thelabtech-remote:
      Near :thelabtech-local:
      Patterns "INBOX" "Sent" "Drafts" "Trash" "Archive"
      Create Near
      Sync Pull

      Channel yahoo
      Far :yahoo-remote:
      Near :yahoo-local:
      Patterns *
      Create Both
      Sync Pull

      Channel frro
      Far :frro-remote:
      Near :frro-local:
      Patterns *
      Create Both
      Sync All

      Channel fceia
      Far :fceia-remote:
      Near :fceia-local:
      Patterns *
      Create Both
      Sync All

      # Group for syncing all accounts at once
      Group gmail
      Channel personal-gmail
      Channel ips
      Channel thelabtech
      Channel yahoo
      Channel frro
      Channel fceia
    '';
  };
}
