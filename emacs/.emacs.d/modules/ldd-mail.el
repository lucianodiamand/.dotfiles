;; -*- lexical-binding: t; -*-

(use-package mu4e
  :ensure nil ; Installed via distro package manager
  ;;:defer 20 ; Wait until 20 seconds after startup
  ;;:defer t  ; Fixit no carga mu4e con esta opcion
  ;;:bind (("C-c m m" . mu4e)
  ;;       ("C-c m c" . 'mu4e-compose-new)
  ;;       ("C-c m i" . 'ldd/go-to-inbox)
  ;;       ("C-c m s" . 'mu4e-update-mail-and-index))
  ;;:hook ((mu4e-compose-mode . turn-off-auto-fill)
  ;;       (mu4e-compose-mode . turn-off-auto-fill)
  ;;       )
  :config
  (require 'mu4e-org)
  ;; Refresh mail using isync every 10 minutes
  ;; NOTE: Trying to refresh mail manually now to increase focus
  ;;(setq mu4e-update-interval (* 10 60))
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-maildir "~/.mail")
  (setq mu4e-date-format-long "%d/%m/%Y")
  (setq mu4e-headers-date-format "%d/%m/%Y")

  ;;(setq mu4e-headers-fields
  ;;      '((:human-date    .  12)   ;; Fecha: dd/mm/yyyy
  ;;        (:flags         .   6)   ;; Marcas (leido, archivado, etc.)
  ;;        (:from          .  25)   ;; Remitente
  ;;        (:subject       .  60)   ;; Asunto
  ;;        (:maildir       .  20))) ;; Carpeta (opcional)

  ;;(setq mu4e-headers-visible-columns 80)

  ;; Use Ivy for mu4e completions (maildir folders, etc)
  (setq mu4e-completing-read-function #'completing-read)

  ;; Make sure that moving a message (like to Trash) causes the
  ;; message to get a new file name.  This helps to avoid the
  ;; dreaded "UID is N beyond highest assigned" error.
  ;; See this link for more info: https://stackoverflow.com/a/43461973
  (setq mu4e-change-filenames-when-moving t)

  ;; Set up contexts for email accounts
  (setq mu4e-contexts
     `( ,(make-mu4e-context
           :name "Personal Gmail"
           :match-func (lambda (msg) (when msg
                                       (string-prefix-p "/personal-gmail" (mu4e-message-field msg :maildir))))
           :vars '((mu4e-maildir-shortcuts . (("/personal-gmail/Inbox" . ?i)
                                              ("/personal-gmail/Sent" . ?s)
                                              ("/personal-gmail/Trash" . ?t)))
                   (user-mail-address      . "lucianodiamand@gmail.com")
                   (user-full-name         . "Luciano Diamand")))

         ,(make-mu4e-context
           :name "IPS Gmail"
           :match-func (lambda (msg) (when msg
                                       (string-prefix-p "/ips" (mu4e-message-field msg :maildir))))
           :vars '((mu4e-maildir-shortcuts . (("/ips/Inbox" . ?i)
                                              ("/ips/Sent" . ?s)
                                              ("/ips/Trash" . ?t)))
                   (user-mail-address      . "ldiamand@ips.edu.ar")
                   (user-full-name         . "Luciano Diamand")))

         ,(make-mu4e-context
           :name "TheLabTech Gmail"
           :match-func (lambda (msg) (when msg
                                       (string-prefix-p "/thelabtech" (mu4e-message-field msg :maildir))))
           :vars '((mu4e-maildir-shortcuts . (("/thelabtech/Inbox" . ?i)
                                              ("/thelabtech/Sent" . ?s)
                                              ("/thelabtech/Trash" . ?t)))
                   (user-mail-address      . "luciano.diamand@thelabtech.com.ar")
                   (user-full-name         . "Luciano Diamand")))

        ,(make-mu4e-context
           :name "Yahoo"
           :match-func (lambda (msg) (when msg
                                       (string-prefix-p "/yahoo" (mu4e-message-field msg :maildir))))
           :vars '((mu4e-maildir-shortcuts . (("/yahoo/Inbox" . ?i)
                                              ("/yahoo/Sent" . ?s)
                                              ("/yahoo/Trash" . ?t)))
                   (user-mail-address      . "lucianodiamand@yahoo.com")
                   (user-full-name         . "Luciano Diamand")))

         ,(make-mu4e-context
           :name "UTN"
           :match-func (lambda (msg) (when msg
                                       (string-prefix-p "/frro" (mu4e-message-field msg :maildir))))
           :vars '((mu4e-maildir-shortcuts . (("/frro/Inbox" . ?i)
                                              ("/frro/Sent" . ?s)
                                              ("/frro/Trash" . ?t)))
                   (user-mail-address      . "ldiamand@frro.utn.edu.ar")
                   (user-full-name         . "Luciano Diamand")))

         ,(make-mu4e-context
           :name "Fceia"
           :match-func (lambda (msg) (when msg
                                       (string-prefix-p "/fceia" (mu4e-message-field msg :maildir))))
           :vars '((mu4e-maildir-shortcuts . (("/fceia/Inbox" . ?i)
                                              ("/fceia/Sent" . ?s)
                                              ("/fceia/Trash" . ?t)))
                   (user-mail-address      . "ldiamand@fceia.unr.edu.ar")
                   (user-full-name         . "Luciano Diamand")))))

  (setq mu4e-context-policy 'pick-first)

  ;; Prevent mu4e from permanently deleting trashed items
  ;; This snippet was taken from the following article:
  ;; http://cachestocaches.com/2017/3/complete-guide-email-emacs-using-mu-and-/
  (defun remove-nth-element (nth list)
    (if (zerop nth) (cdr list)
      (let ((last (nthcdr (1- nth) list)))
        (setcdr last (cddr last))
        list)))

  (setf (alist-get 'trash mu4e-marks)
        '(:char ("d" . "▼")
                :prompt "dtrash"
                :dyn-target (lambda (target msg) (mu4e-get-trash-folder msg))
                :action (lambda (docid msg target)
                          (mu4e--server-move docid
                                             (mu4e--mark-check-target target) "-N"))))

  ;; Display options
  (setq mu4e-view-show-images t)
  (setq mu4e-view-show-addresses 't)

  ;; Composing mail
  (setq mu4e-compose-dont-reply-to-self t)

  ;; 
  (setq mu4e-update-interval 300)            ;; Update every 5 minutes
  (setq mu4e-change-filenames-when-moving t) ;; Avoid filename collisions
  (setq mu4e-view-show-images t)             ;; Show inline images
  (setq mu4e-view-prefer-html t)            ;; Prefer HTML emails
  (setq mu4e-compose-signature-auto-include nil) ;; No automatic signature
  (setq mu4e-context-policy 'pick-first)     ;; Start with the first context
  (setq mu4e-compose-context-policy 'ask)  ;; Ask for context when composing

  ;; Set SMTP
  (require 'smtpmail)
  (setq send-mail-function 'sendmail-send-it
      message-send-mail-function 'message-send-mail-with-sendmail
      sendmail-program "msmtp"
      message-sendmail-extra-arguments '("--read-envelope-from")
      message-sendmail-f-is-evil t)

  (setq org-capture-templates
      `(("m" "Email Workflow")
        ("mf" "Follow Up" entry (file+olp "~/dev/org/mail.org" "Follow Up")
         "* TODO %a")
        ("mr" "Read Later" entry (file+olp "~/dev/org/mail.org" "Read Later")
         "* TODO %a")))

  ;;(add-hook 'mu4e-headers-mode-hook
  ;;          (lambda ()
  ;;            (setq buffer-face-mode-face '(:family "Monospacce"))
  ;;            (buffer-face-mode)))

  ;; Use mu4e for sending e-mail
  ;;(setq mail-user-agent 'mu4e-user-agent
  ;;      message-send-mail-function 'smtpmail-send-it
  ;;      smtpmail-smtp-server "smtp.fastmail.com"
  ;;      smtpmail-smtp-service 465
  ;;      smtpmail-stream-type 'ssl)

  ;; Signing messages (use mml-secure-sign-pgpmime)
  ;;LDD(setq mml-secure-openpgp-signers '("3A74994A3EB078555A0FFDD599F6A2219E3A3C44"))

  ;; (See the documentation for `mu4e-sent-messages-behavior' if you have
  ;; additional non-Gmail addresses and want assign them different
  ;; behavior.)

  ;; setup some handy shortcuts
  ;; you can quickly switch to your Inbox -- press ``ji''
  ;; then, when you want archive some messages, move them to
  ;; the 'All Mail' folder by pressing ``ma''.
  ;;(setq mu4e-maildir-shortcuts
  ;;      '(("/Fastmail/INBOX" . ?i)
  ;;        ("/Fastmail/Archive" . ?a)
  ;;        ("/Fastmail/Lists/*" . ?l)
  ;;        ("/Fastmail/Sent Items" . ?s)
  ;;        ("/Fastmail/Trash" . ?t)))

  ;;(add-to-list 'mu4e-bookmarks
  ;;             '(:name "All Inboxes"
  ;;                     :query "maildir:/Fastmail/INBOX"
  ;;                     :key ?i))

  ;; don't keep message buffers around
  (setq message-kill-buffer-on-exit t)

  ;;(setq ldd/mu4e-inbox-query
  ;;      "(maildir:/Fastmail/INBOX) AND flag:unread")

  (defun ldd/go-to-inbox ()
    (interactive)
    (mu4e-headers-search ldd/mu4e-inbox-query))

  ;; Start mu4e in the background so that it syncs mail periodically
  (mu4e t))

;;(use-package mu4e-alert
;;  :after mu4e
;;  :config
;;  ;; Show unread emails from all inboxes
;;  (setq mu4e-alert-interesting-mail-query
;;        (concat ldd/mu4e-inbox-query
;;                " date:30M..now"))
;;
;;  ;; Show notifications for mails already notified
;;  (setq mu4e-alert-notify-repeated-mails nil)
;;
;;  (mu4e-alert-enable-notifications))

(provide 'ldd-mail)
