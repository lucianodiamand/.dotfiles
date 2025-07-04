;; -*- lexical-binding: t; -*-

(use-package pdf-tools
  :config
  (pdf-tools-install)) ;; Activa visor de PDF

;; (with-eval-after-load 'org-noter
;;   (setq org-noter-enabled-modes '(pdf-view-mode)))

(use-package org-noter
  :after pdf-tools
  :config
  (setq org-noter-notes-search-path '("~/org/roam")))

(provide 'ldd-pdf-tools)

