;; -*- lexical-binding: t; -*-

;; === TODO keywords y faces ===
(setq org-todo-keywords
      '((sequence
         "IDEA(i)" "TODO(t)" "NEXT(n)" "WAIT(w@/!)" "|" "DONE(d)" "CANCELLED(c@)")))
(setq org-todo-keyword-faces
      '(("IDEA" . font-lock-doc-face)
        ("NEXT" . (:weight bold))
        ("WAIT" . warning)
        ("CANCELLED" . shadow)))

;; === Tags rápidas útiles ===
(setq org-tag-alist
      '((:startgroup . nil)
        ("@pc" . ?p) ("@lab" . ?l) ("@casa" . ?c)
        (:endgroup . nil)
        ("bloqueado" . ?b) ("urgente" . ?u) ("low" . ?o)
        ("ref" . ?r) ("nota" . ?n) ("proyecto" . ?j) ("reunion" . ?m)))

;; === Agenda: tus carpetas clave ===
(setq org-agenda-files '("~/org/roam/proyectos/"
                         "~/org/roam/versiones/"
                         "~/org/roam/tareas/"
                         "~/org/roam/reuniones/"
                         "~/org/roam/dailies/"))

;; === Refile: enviar items a Proyectos/Tareas/Notas ===
(setq org-refile-targets
      '(("~/org/roam/proyectos" :maxlevel . 3)
        ("~/org/roam/tareas"    :maxlevel . 2)
        ("~/org/roam/notas"     :maxlevel . 2)))
(setq org-outline-path-complete-in-steps nil
      org-refile-use-outline-path t)

;; === Agenda custom views (GTD-ish) ===
(setq org-agenda-custom-commands
      '(("d" "Hoy + NEXT"
         ((agenda "" ((org-agenda-span 1)))
          (todo "NEXT")
          (todo "WAIT"))
         ((org-agenda-overriding-header "📅 Hoy / NEXT / WAIT")))
        ("w" "Semana + proyectos activos"
         ((agenda "" ((org-agenda-span 7)))
          (tags-todo "+proyecto+TODO|+proyecto+NEXT"))
         ((org-agenda-overriding-header "🗓️ Semana & Proyectos")))
        ("i" "Inbox sin programar"
         ((todo "TODO"
                ((org-agenda-skip-function
                  '(org-agenda-skip-entry-if 'scheduled 'deadline)))))
         ((org-agenda-overriding-header "📥 Inbox")))))

(with-eval-after-load 'org
  (global-set-key (kbd "C-c c") #'org-capture))

(provide 'ldd-org-gtd)
