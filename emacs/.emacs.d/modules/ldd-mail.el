;; -*- lexical-binding: t; -*-

(defvar ldd/mail-account-alist
  '(("lucianodiamand@gmail.com"
     :shortname "personal-gmail"
     :full-name "Luciano Diamand"
     :maildir "personal-gmail"
     :inbox "/personal-gmail/Inbox"
     :sent "/persona-gmail/Sent"
     :drafts "/personal-gmail/Drafts"
     :trash "/personal-gmail/Trash"
     :archive "/personal-gmail/Archive")

    ("ldiamand@ips.edu.ar"
     :shortname "ips"
     :full-name "Luciano Diamand"
     :maildir "ips"
     :inbox "/ips/Inbox"
     :sent "/ips/Sent"
     :drafts "/ips/Drafts"
     :trash "/ips/Trash"
     :archive "/ips/Archive")

    ("luciano.diamand@thelabtech.com.ar"
     :shortname "thelabtech"
     :full-name "Luciano Diamand"
     :maildir "thelabtech"
     :inbox "/thelabtech/Inbox"
     :sent "/thelabtech/Sent"
     :drafts "/thelabtech/Drafts"
     :trash "/thelabtech/Trash"
     :archive "/thelabtech/Archive")

    ("lucianodiamand@yahoo.com"
     :shortname "yahoo"
     :full-name "Luciano Diamand"
     :maildir "yahoo"
     :inbox "/yahoo/Inbox"
     :sent "/yahoo/Sent"
     :drafts "/yahoo/Drafts"
     :trash "/yahoo/Trash"
     :archive "/yahoo/Archive")

    ("ldiamand@frro.utn.edu.ar"
     :shortname "frro"
     :full-name "Luciano Diamand"
     :maildir "frro"
     :inbox "/frro/Inbox"
     :sent "/frro/Sent"
     :drafts "/frro/Drafts"
     :trash "/frro/Trash"
     :archive "/frro/Archive")

    ("ldiamand@fceia.unr.edu.ar"
     :shortname "fceia"
     :full-name "Luciano Diamand"
     :maildir "fceia"
     :inbox "/fceia/Inbox"
     :sent "/fceia/Sent"
     :drafts "/fceia/Drafts"
     :trash "/fceia/Trash"
     :archive "/fceia/Archive")))

(global-set-key (kbd "C-c m i") #'ldd/go-to-inbox)
(global-set-key (kbd "C-c m c") #'mu4e-context-switch)
(global-set-key (kbd "C-c m C") #'ldd/mu4e-context-switch-and-show-inbox)

;; Definir consultas personalizadas
(defvar ldd/mu4e-inbox-query
  "maildir:/personal-gmail/Inbox OR maildir:/ips/Inbox OR maildir:/thelabtech/Inbox OR maildir:/yahoo/Inbox OR maildir:/frro/Inbox OR maildir:/fceia/Inbox")

(defun ldd/go-to-inbox ()
  (interactive)
  (mu4e-headers-search ldd/mu4e-inbox-query))

(defun ldd/load-and-process-aliases ()
  "Cargar y procesar aliases correctamente para mu4e"
  (ldd/load-email-aliases)
  (setq mu4e-alias-list
        (mapcar (lambda (alias)
                  (cons (car alias)
                        (split-string (cdr alias) ",\\s *" t)))
                email-aliases))
  ;; Actualizar la lista que usa mu4e para completado
  (setq mu4e-alias-define-list mu4e-alias-list)
  (setq mu4e-compose-complete-aliases t))

;; Función para cargar aliases de correo
(defun ldd/load-email-aliases ()
  "Cargar alias de correo desde archivos .el en ~/.config/email/aliases/"
  (let ((alias-dir "~/.config/email/aliases/"))
    (setq email-aliases '())  ; Inicializar variable
    
    (when (file-directory-p alias-dir)
      (dolist (file (directory-files alias-dir t "\\.el$"))
        (when (file-readable-p file)
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            ;; Leer la lista de aliases directamente
            (let ((aliases (read (current-buffer))))
              (setq email-aliases (append email-aliases aliases)))))))))

;; Función para procesar los aliases cargados
(defun ldd/process-email-aliases ()
  "Procesar los alias cargados y prepararlos para mu4e"
  (setq mu4e-alias-list
        (when (boundp 'email-aliases)
          (mapcar (lambda (alias)
                    (cons (car alias)
                          (split-string (cdr alias) ",\\s *" t)))
                  email-aliases))))

;; Cargar y procesar aliases al inicio
(ldd/load-email-aliases)
(ldd/process-email-aliases)

;; Función para expandir aliases al componer correos
(defun ldd/mu4e-alias-expand (name)
  "Expandir alias al componer correos"
  (when mu4e-alias-list
    (let ((alias (assoc name mu4e-alias-list)))
      (if alias (cdr alias) name))))

(use-package mu4e
  :ensure nil ; Installed via distro package manager
  ;;:defer 20 ; Wait until 20 seconds after startup
  ;;:defer t  ; Fixit no carga mu4e con esta opcion
  ;;:bind (("C-c m m" . mu4e)
  ;;       ("C-c m c" . 'mu4e-compose-new)
  ;;       ("C-c m i" . 'ldd/go-to-inbox)
  ;;       ("C-c m s" . 'mu4e-update-mail-and-index))
  ;;:hook (mu4e-compose-mode . (lambda () (buffer-face-set 'fixed-pitch)))
  :config
  (require 'mu4e-org)
  ;; Refresh mail using isync every 10 minutes
  ;; NOTE: Trying to refresh mail manually now to increase focus
  ;;(setq mu4e-update-interval (* 10 60))
  (setq mu4e-update-interval nil)
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-maildir "~/.mail")
  (setq mu4e-date-format-long "%d/%m/%Y")
  (setq mu4e-headers-date-format "%d/%m/%Y")

  (setq mu4e-headers-fields
        '((:date          .  12)
          (:flags         .   6)
          (:from          .  25)
          (:subject       . nil)))

  (add-hook 'mu4e-headers-mode-hook (lambda () (display-line-numbers-mode -1)))
  (add-hook 'mu4e-view-mode-hook (lambda () (display-line-numbers-mode -1)))
  ;;(setq mu4e-headers-visible-columns 80)

  ;; Esto ayuda a que los aliases tengan prioridad en el completado
  (setq mu4e-compose-complete-first 'aliases)

  ;; Usa esto si quieres ver los aliases expandidos al terminar de escribir
  (defun ldd/mu4e-alias-expand-on-tab ()
    "Expandir alias al presionar TAB"
    (interactive)
    (let ((alias (mu4e-alias-expand-names (mu4e-alias-which-alias))))
      (when alias
        (delete-region (line-beginning-position) (line-end-position))
        (insert alias))))

  ;; Funciones para manejo de aliases
  (defun ldd/mu4e-complete-alias ()
    "Completado manual de aliases"
    (interactive)
    (let ((completion-ignore-case t)
          (aliases (mapcar 'car mu4e-alias-list)))
      (insert (completing-read "Alias: " aliases nil nil nil nil nil nil t))))

  (defun ldd/mu4e-alias-expand-on-tab ()
    "Expandir alias al presionar TAB"
    (interactive)
    (let* ((end (point))
           (beg (save-excursion
                  (re-search-backward "\\(\\`\\|[\n:,]\\)[ \t]*")
                  (match-end 0)))
           (alias (buffer-substring-no-properties beg end))
           (expansion (cdr (assoc alias mu4e-alias-list))))
      (when expansion
        (delete-region beg end)
        (insert (mapconcat 'identity expansion ", ")))))

  ;; Asignación de teclas
  (define-key mu4e-compose-mode-map (kbd "C-c a") #'ldd/mu4e-complete-alias)
  (define-key mu4e-compose-mode-map (kbd "<tab>") #'ldd/mu4e-alias-expand-on-tab)

  ;; Use Ivy for mu4e completions (maildir folders, etc)
  (setq mu4e-completing-read-function #'completing-read)

  ;; Make sure that moving a message (like to Trash) causes the
  ;; message to get a new file name.  This helps to avoid the
  ;; dreaded "UID is N beyond highest assigned" error.
  ;; See this link for more info: https://stackoverflow.com/a/43461973
  (setq mu4e-change-filenames-when-moving t)

  (setq mu4e-user-mail-address-list '("lucianodiamand@gmail.com" "lucianodiamand@yahoo.com" "ldiamand@ips.edu.ar" "ldiamand@frro.utn.edu.ar" "ldiamand@fceia.unr.edu.ar" "luciano.diamand@thelabtech.com.ar"))

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
                   (user-full-name         . "Luciano Diamand")
                   (mu4e-sent-folder   . "/personal-gmail/Sent")
                   (mu4e-drafts-folder . "/personal-gmail/Drafts")
                   (mu4e-trash-folder  . "/personal-gmail/Trash")
                   (mu4e-refile-folder . "/personal-gmail/Archive")))

         ,(make-mu4e-context
           :name "IPS Gmail"
           :match-func (lambda (msg) (when msg
                                       (string-prefix-p "/ips" (mu4e-message-field msg :maildir))))
           :vars '((mu4e-maildir-shortcuts . (("/ips/Inbox" . ?i)
                                              ("/ips/Sent" . ?s)
                                              ("/ips/Trash" . ?t)))
                   (user-mail-address      . "ldiamand@ips.edu.ar")
                   (user-full-name         . "Luciano Diamand")
                   (mu4e-sent-folder   . "/ips/Sent")
                   (mu4e-drafts-folder . "/ips/Drafts")
                   (mu4e-trash-folder  . "/ips/Trash")
                   (mu4e-refile-folder . "/ips/Archive")))

         ,(make-mu4e-context
           :name "TheLabTech Gmail"
           :match-func (lambda (msg) (when msg
                                       (string-prefix-p "/thelabtech" (mu4e-message-field msg :maildir))))
           :vars '((mu4e-maildir-shortcuts . (("/thelabtech/Inbox" . ?i)
                                              ("/thelabtech/Sent" . ?s)
                                              ("/thelabtech/Trash" . ?t)))
                   (user-mail-address      . "luciano.diamand@thelabtech.com.ar")
                   (user-full-name         . "Luciano Diamand")
                   (mu4e-sent-folder   . "/thelabtech/Sent")
                   (mu4e-drafts-folder . "/thelabtech/Drafts")
                   (mu4e-trash-folder  . "/thelabtech/Trash")
                   (mu4e-refile-folder . "/thelabtech/Archive")))

        ,(make-mu4e-context
           :name "Yahoo"
           :match-func (lambda (msg) (when msg
                                       (string-prefix-p "/yahoo" (mu4e-message-field msg :maildir))))
           :vars '((mu4e-maildir-shortcuts . (("/yahoo/Inbox" . ?i)
                                              ("/yahoo/Sent" . ?s)
                                              ("/yahoo/Trash" . ?t)))
                   (user-mail-address      . "lucianodiamand@yahoo.com")
                   (user-full-name         . "Luciano Diamand")
                   (mu4e-sent-folder   . "/yahoo/Sent")
                   (mu4e-drafts-folder . "/yahoo/Drafts")
                   (mu4e-trash-folder  . "/yahoo/Trash")
                   (mu4e-refile-folder . "/yahoo/Archive")))

         ,(make-mu4e-context
           :name "UTN"
           :match-func (lambda (msg) (when msg
                                       (string-prefix-p "/frro" (mu4e-message-field msg :maildir))))
           :vars '((mu4e-maildir-shortcuts . (("/frro/Inbox" . ?i)
                                              ("/frro/Sent" . ?s)
                                              ("/frro/Trash" . ?t)))
                   (user-mail-address      . "ldiamand@frro.utn.edu.ar")
                   (user-full-name         . "Luciano Diamand")
                   (mu4e-sent-folder   . "/frro/Sent")
                   (mu4e-drafts-folder . "/frro/Drafts")
                   (mu4e-trash-folder  . "/frro/Trash")
                   (mu4e-refile-folder . "/frro/Archive")))

         ,(make-mu4e-context
           :name "Fceia"
           :match-func (lambda (msg) (when msg
                                       (string-prefix-p "/fceia" (mu4e-message-field msg :maildir))))
           :vars '((mu4e-maildir-shortcuts . (("/fceia/Inbox" . ?i)
                                              ("/fceia/Sent" . ?s)
                                              ("/fceia/Trash" . ?t)))
                   (user-mail-address      . "ldiamand@fceia.unr.edu.ar")
                   (user-full-name         . "Luciano Diamand")
                   (mu4e-sent-folder   . "/fceia/Sent")
                   (mu4e-drafts-folder . "/fceia/Drafts")
                   (mu4e-trash-folder  . "/fceia/Trash")
                   (mu4e-refile-folder . "/fceia/Archive")))))

  ;; Prevent mu4e from permanently deleting trashed items
  ;; This snippet was taken from the following article:
  ;; http://cachestocaches.com/2017/3/complete-guide-email-emacs-using-mu-and-/
  (defun remove-nth-element (nth list)
    (if (zerop nth) (cdr list)
      (let ((last (nthcdr (1- nth) list)))
        (setcdr last (cddr last))
        list)))

  (defun ldd/mu4e-context-switch-and-show-inbox ()
  "Cambiar contexto y mostrar la bandeja de entrada correspondiente."
  (interactive)
  (mu4e-context-switch)
  (let ((ctx (mu4e-context-current)))
    (when ctx
      (let ((address (cdr (assoc 'user-mail-address (mu4e-context-vars ctx)))))
        (cond
         ((string= address "lucianodiamand@gmail.com") (mu4e-headers-search "maildir:/personal-gmail/Inbox"))
         ((string= address "ldiamand@ips.edu.ar")      (mu4e-headers-search "maildir:/ips/Inbox"))
         ((string= address "luciano.diamand@thelabtech.com.ar") (mu4e-headers-search "maildir:/thelabtech/Inbox"))
         ((string= address "lucianodiamand@yahoo.com") (mu4e-headers-search "maildir:/yahoo/Inbox"))
         ((string= address "ldiamand@frro.utn.edu.ar") (mu4e-headers-search "maildir:/frro/Inbox"))
         ((string= address "ldiamand@fceia.unr.edu.ar") (mu4e-headers-search "maildir:/fceia/Inbox")))))))
  
  (defun ldd/set-msmtp-account ()
    (setq message-sendmail-envelope-from 'header)
    (let ((from (message-field-value "From")))
      (when (and from (stringp from))
        (setq message-sendmail-extra-arguments
              (list "--read-envelope-from"
                    (concat "--account=" (cond
                                          ((string-match "lucianodiamand@gmail.com" from) "personal-gmail")
                                          ((string-match "ldiamand@ips.edu.ar" from) "ips")
                                          ((string-match "luciano.diamand@thelabtech.com.ar" from) "thelabtech")
                                          ((string-match "lucianodiamand@yahoo.com" from) "yahoo")
                                          ((string-match "ldiamand@frro.utn.edu.ar" from) "frro")
                                          ((string-match "ldiamand@fceia.unr.edu.ar" from) "fceia")
                                          (t "default"))))))))
  (add-hook 'message-send-mail-hook #'ldd/set-msmtp-account)

  (setf (alist-get 'trash mu4e-marks)
        '(:char ("d" . "▼")
                :prompt "dtrash"
                :dyn-target (lambda (target msg) (mu4e-get-trash-folder msg))
                :action (lambda (docid msg target)
                          (mu4e--server-move docid
                                             (mu4e--mark-check-target target) "-N"))))

  ;; Composing mail
  (setq mu4e-compose-dont-reply-to-self t)

  ;; Display options
  (setq mu4e-view-show-addresses 't)
  ;;(setq mu4e-update-interval 300)            ;; Update every 5 minutes
  (setq mu4e-change-filenames-when-moving t) ;; Avoid filename collisions
  (setq mu4e-view-show-images t)             ;; Show inline images
  (setq mu4e-view-prefer-html t)             ;; Prefer HTML emails
  (setq mu4e-compose-signature-auto-include nil) ;; No automatic signature
  (setq mu4e-context-policy 'pick-first)     ;; Start with the first context
  (setq mu4e-compose-context-policy 'ask)    ;; Ask for context when composing

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

  ;; Start mu4e in the background so that it syncs mail periodically
  ;;(mu4e t)
  )

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
