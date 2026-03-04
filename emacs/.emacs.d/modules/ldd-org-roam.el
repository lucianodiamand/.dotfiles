;; -*- lexical-binding: t; -*-

(defvar ldd/org-roam-dir (file-truename (expand-file-name "~/org/roam")))

(defvar ldd/roam-notes "notes/")
(defvar ldd/roam-notes "projects/")
(defvar ldd/roam-notes "tasks/")
(defvar ldd/roam-notes "meetings/")
(defvar ldd/roam-notes "versions/")
(defvar ldd/roam-notes "slides/")

(use-package org-roam
  :custom
  (org-roam-directory ldd/org-roam-dir)
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
          ;;("d" "Default" plain "%?"
          ;; :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
          ;;                    "#+title: ${title}\n")
          ;; :unnarrowed t)

          ;; Nota técnica / referencia
          ("n" "Nota/Referencia" plain
           "* Contexto\n%?\n\n* Detalles\n\n* Enlaces\n\n"
           :target (file+head "notes/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: :note:ref:\n")
           :unnarrowed t)

          ("b" "Book" plain
           "* Metadata\n- Author: %?\n- Started: %U\n- Status: Reading\n\n* Key Concepts\n\n* Notes\n\n* Related\n"
           :target (file+head "notes/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: :book:\n")
           :unnarrowed t)

          ("c" "Concept" plain
           "* Definition\n%?\n\n* Explanation\n\n* Example\n\n* Related\n"
           :target (file+head "notes/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: :note:concept:\n")
           :unnarrowed t)

          ;; Project (archivo)
          ("p" "Project" plain
           (file ,(expand-file-name "templates/project.tpl" ldd/org-roam-dir))  ;; (ver más abajo el contenido)
           :target (file+head "projects/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: Project - ${title}\n#+filetags: :project:\n")
           :unnarrowed t)

          ;; Tarea suelta (como nota en Tareas/)
          ("t" "Task" plain
           "* TODO %^{Descripción}\n:PROPERTIES:\n:Created: %U\n:END:\n%?\n"
           :target (file+head "tasks/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: :task:\n")
           :unnarrowed t)

          ;; Reunión
          ("m" "Meeting" plain
           (file ,(expand-file-name "templates/meeting.tpl" ldd/org-roam-dir))
           :target (file+head "meetings/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: :meeting:\n")
           :unnarrowed t)

          ;; Versión / Release notes
          ("v" "Version/Release" plain
           (file ,(expand-file-name "templates/version.tpl" ldd/org-roam-dir))
           :target (file+head "versions/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: :version:\n")
           :unnarrowed t)

          ;; Slides (para org-present)
          ("s" "Slides" plain
           (file ,(expand-file-name "templates/slide.tpl" ldd/org-roam-dir))
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

(with-eval-after-load 'org
  (org-babel-do-load-languages
    'org-babel-load-languages
    '((shell . t)
      (java . t))))

(with-eval-after-load 'org
  (setq org-src-preserve-indentation t
    org-edit-src-content-indentation 0))

(provide 'ldd-org-roam)

