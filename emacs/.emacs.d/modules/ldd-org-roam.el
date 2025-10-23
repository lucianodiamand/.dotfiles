;; -*- lexical-binding: t; -*-

(use-package org-roam
  :custom
  (org-roam-directory "~/org/roam")
  (org-roam-dailies-directory "dailies/")
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
        `(
          ;; Default mínimo
          ("d" "Default" plain "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t)

          ;; Nota técnica / referencia
          ("n" "Nota/Referencia" plain
           "* Contexto\n%?\n\n* Detalles\n\n* Enlaces\n\n"
           :target (file+head "notas/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: :nota:ref:\n")
           :unnarrowed t)

          ;; Proyecto (archivo)
          ("p" "Proyecto" plain
           (file "templates/project.tpl")  ;; (ver más abajo el contenido)
           :target (file+head "proyectos/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: :proyecto:\n#+ROAM_ALIASES: \n")
           :unnarrowed t)

          ;; Tarea suelta (como nota en Tareas/)
          ("t" "Tarea" plain
           "* TODO %^{Descripción}\n:PROPERTIES:\n:Created: %U\n:END:\n%?\n"
           :target (file+head "tareas/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: :tarea:\n")
           :unnarrowed t)

          ;; Reunión
          ("m" "Reunión" plain
           (file "templates/metting.tpl")
           :target (file+head "reuniones/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: :reunion:\n")
           :unnarrowed t)

          ;; Versión / Release notes
          ("v" "Versión/Release" plain
           (file "templates/version.tpl")
           :target (file+head "versiones/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: :version:\n")
           :unnarrowed t)

          ;; Slides (para org-present)
          ("s" "Slides" plain
           (file "templates/slide.tpl")
           :target (file+head "slides/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: :slides:\n")
           :unnarrowed t)
          ))

  ;; === Dailies templates ===
  (setq org-roam-dailies-capture-templates
        '(("j" "Daily" entry
           "* Plan\n- [ ] %?\n\n* Log\n- %U\n\n* Wins\n- "
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n#+filetags: :daily:\n"))))

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

