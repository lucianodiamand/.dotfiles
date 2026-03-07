{ config, pkgs, lib, secrets, ... }:

{
  sops = {
    defaultSopsFile = "${secrets}/home.sops.yaml";
    age.keyFile = "${config.home.homeDirectory}/.config/sops/age/keys.txt";
    secrets = {
      email_personal_gmail = {};
      email_ips = {};
      email_thelabtech = {};
      email_fceia = {};
      email_frro = {};
      email_yahoo = {};

      user_fceia = {};
      user_frro = {};

      smtp_host_gmail = {};
      smtp_host_fceia = {};
      smtp_host_frro = {};
      smtp_host_yahoo = {};

      smtp_port_gmail = {};
      smtp_port_fceia = {};
      smtp_port_frro = {};
      smtp_port_yahoo = {};

      imap_host_gmail = {};
      imap_host_yahoo = {};
      imap_host_frro = {};
      imap_host_fceia = {};

      mail_root = {};

      ssh_bitbucket_host = {};
      ssh_cptprod_host = {};
      ssh_cptprod_port = {};
      ssh_github_host = {};
      ssh_arqdesa_host = {};
      ssh_arqdesa_user = {};
      ssh_arqdesa_port = {};
      ssh_sunde_host = {};

      git_email_thelabtech = {};
      git_email_aus = {};
      git_email_personal = {};

      git_user_name_thelabtech = {};
      git_user_name_aus = {};
      git_user_name_personal = {};
    };

    templates = {
      "mu_cfg" = {
        content = ''
          [mu]
          maildir=${config.sops.placeholder.mail_root}
        '';
      };

      "msmtprc" = {
        mode = "0600";
        content = ''
          # Account for Personal Gmail
          account personal-gmail
          host ${config.sops.placeholder.smtp_host_gmail}
          port ${config.sops.placeholder.smtp_port_gmail}
          from ${config.sops.placeholder.email_personal_gmail}
          auth on
          user ${config.sops.placeholder.email_personal_gmail}
          passwordeval "pass emails/gmail_mu4e"
          tls on
          tls_starttls off
          logfile ~/.msmtp.log

          # Account for Ips
          account ips
          host ${config.sops.placeholder.smtp_host_gmail}
          port ${config.sops.placeholder.smtp_port_gmail}
          from ${config.sops.placeholder.email_ips}
          auth on
          user ${config.sops.placeholder.email_ips}
          passwordeval "pass emails/ips_mu4e"
          tls on
          tls_starttls off
          logfile ~/.msmtp.log

          # Account for thelabtech
          account thelabtech
          host ${config.sops.placeholder.smtp_host_gmail}
          port ${config.sops.placeholder.smtp_port_gmail}
          from ${config.sops.placeholder.email_thelabtech}
          auth on
          user ${config.sops.placeholder.email_thelabtech}
          passwordeval "pass emails/thelab_mu4e"
          tls on
          tls_starttls off
          logfile ~/.msmtp.log

          # Account for fceia
          account fceia
          host ${config.sops.placeholder.smtp_host_fceia}
          port ${config.sops.placeholder.smtp_port_fceia}
          from ${config.sops.placeholder.email_fceia}
          auth plain
          user ${config.sops.placeholder.user_fceia}
          passwordeval "pass emails/fceia"
          tls on
          # Enable STARTTLS if port 587 is used; set to "off" if port 465 is used
          tls_starttls off
          logfile ~/.msmtp.log

          # Account for utn
          account frro
          host ${config.sops.placeholder.smtp_host_frro}
          port ${config.sops.placeholder.smtp_port_frro}
          from ${config.sops.placeholder.email_frro}
          auth on
          user ${config.sops.placeholder.user_frro}
          passwordeval "pass emails/utn"
          tls on
          # Enable STARTTLS if port 587 is used; set to "off" if port 465 is used
          tls_starttls on
          logfile ~/.msmtp.log

          # Account for yahoo
          account yahoo
          host ${config.sops.placeholder.smtp_host_yahoo}
          port ${config.sops.placeholder.smtp_port_yahoo}
          from ${config.sops.placeholder.email_yahoo}
          auth on
          user ${config.sops.placeholder.email_yahoo}
          passwordeval "pass emails/yahoo_mu4e"
          tls on
          # Enable STARTTLS if port 587 is used; set to "off" if port 465 is used
          tls_starttls on
          logfile ~/.msmtp.log

          # Default account
          account default : frro
        '';
      };

      "mbsyncrc" = {
        mode = "0600";
        content = ''
          # General settings
          IMAPAccount personal-gmail
          Host ${config.sops.placeholder.imap_host_gmail}
          User ${config.sops.placeholder.email_personal_gmail}
          PassCmd "pass emails/gmail_mu4e"
          TLSType IMAPS
          AuthMechs LOGIN

          IMAPAccount ips
          Host ${config.sops.placeholder.imap_host_gmail}
          User ${config.sops.placeholder.email_ips}
          PassCmd "pass emails/ips_mu4e"
          TLSType IMAPS
          AuthMechs LOGIN

          IMAPAccount thelabtech
          Host ${config.sops.placeholder.imap_host_gmail}
          User ${config.sops.placeholder.email_thelabtech}
          PassCmd "pass emails/thelab_mu4e"
          TLSType IMAPS
          AuthMechs LOGIN

          IMAPAccount yahoo
          Host ${config.sops.placeholder.imap_host_yahoo}
          User ${config.sops.placeholder.email_yahoo}
          PassCmd "pass emails/yahoo_mu4e"
          TLSType IMAPS
          PipelineDepth 1

          IMAPAccount frro
          Host ${config.sops.placeholder.imap_host_frro}
          User ${config.sops.placeholder.user_frro}
          PassCmd "pass emails/utn"
          TLSType STARTTLS

          IMAPAccount fceia
          Host ${config.sops.placeholder.imap_host_fceia}
          User ${config.sops.placeholder.user_fceia}
          PassCmd "pass emails/fceia"
          TLSType STARTTLS

          # Local maildir storage
          MaildirStore personal-gmail-local
          Path ${config.sops.placeholder.mail_root}/personal-gmail/
          Inbox ${config.sops.placeholder.mail_root}/personal-gmail/Inbox
          Subfolders Verbatim

          MaildirStore ips-local
          Path ${config.sops.placeholder.mail_root}/ips/
          Inbox ${config.sops.placeholder.mail_root}/ips/Inbox
          Subfolders Verbatim

          MaildirStore thelabtech-local
          Path ${config.sops.placeholder.mail_root}/thelabtech/
          Inbox ${config.sops.placeholder.mail_root}/thelabtech/Inbox
          Subfolders Verbatim

          MaildirStore yahoo-local
          Path ${config.sops.placeholder.mail_root}/yahoo/
          Inbox ${config.sops.placeholder.mail_root}/yahoo/Inbox
          SubFolders Verbatim

          MaildirStore frro-local
          Path ${config.sops.placeholder.mail_root}/frro/
          Inbox ${config.sops.placeholder.mail_root}/frro/Inbox
          SubFolders Verbatim

          MaildirStore fceia-local
          Path ${config.sops.placeholder.mail_root}/fceia/
          Inbox ${config.sops.placeholder.mail_root}/fceia/Inbox
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

      "ssh_config" = {
        mode = "0600";
        content = ''
          Host bitbucket-l2
            HostName ${config.sops.placeholder.ssh_bitbucket_host}
            IdentityFile ~/.ssh/bitbucket.l2.repo.key
            IdentitiesOnly yes

          Host cptprod
            HostName ${config.sops.placeholder.ssh_cptprod_host}
            IdentityFile ~/.ssh/cpt-prod.server.key
            Port ${config.sops.placeholder.ssh_cptprod_port}

          Host ${config.sops.placeholder.ssh_github_host}
            IdentityFile ~/.ssh/github.personal.repo.key

          Host arqdesa
            HostName ${config.sops.placeholder.ssh_arqdesa_host}
            User ${config.sops.placeholder.ssh_arqdesa_user}
            IdentityFile ~/.ssh/arq-desa.server.key
            Port ${config.sops.placeholder.ssh_arqdesa_port}

          Host sunde
            HostName ${config.sops.placeholder.ssh_sunde_host}
            HostKeyAlgorithms +ssh-rsa
            PubkeyAcceptedAlgorithms +ssh-rsa
            SetEnv TERM=rxvt
        '';
      };

      "gitconfig_thelabtech" = {
        content = ''
          [user]
            name = ${config.sops.placeholder.git_user_name_thelabtech}
            email = ${config.sops.placeholder.git_email_thelabtech}
          [color]
            ui = always
        '';
      };

      "gitconfig_aus" = {
        content = ''
          [user]
            name = ${config.sops.placeholder.git_user_name_aus}
            email = ${config.sops.placeholder.git_email_aus}
        '';
      };

      "gitconfig_personal" = {
        content = ''
          [user]
            name = ${config.sops.placeholder.git_user_name_personal}
            email = ${config.sops.placeholder.git_email_personal}
        '';
      };
    };
  };
}
