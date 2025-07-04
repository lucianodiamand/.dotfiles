;; -*- lexical-binding: t; -*-

(use-package org-roam
  :custom
  (org-roam-directory "~/org/roam")
  (org-agenda-files '("~/org/roam/proyectos/"
                      "~/org/roam/versiones/"
                      "~/org/roam/tareas/"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; Asegurar que el directorio exista antes de usarlo
  (unless (file-directory-p org-roam-directory)
    (make-directory org-roam-directory t))

  ;; Templates de captura
  (setq org-roam-capture-templates
        '(("d" "Default" plain
           "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t)
          ("r" "Referencia" plain
           "* Referencia\n\n%?\n\n* Notas\n\n"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+roam_tags: referencia\n")
           :unnarrowed t)))

  ;; Ahora sí: iniciar org-roam
  (org-roam-db-autosync-mode))

(use-package org-roam-ui
  :after org-roam
  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t))

(provide 'ldd-org-roam)

