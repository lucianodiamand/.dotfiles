;; -*- lexical-binding: t; -*-

;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
;; (setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
;;       url-history-file (expand-file-name "url/history" user-emacs-directory))

(with-eval-after-load 'org
  (setq org-startup-indent t))

;; Use no-littering to automatically set common paths to the new user-emacs-directory
(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(provide 'ldd-org)

