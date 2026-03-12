;;; ldd-present.el --- Presentaciones con Org Mode y org-present -*- lexical-binding: t; -*-

(require 'org)
(require 'org-element)
(require 'org-present nil t)
(require 'visual-fill-column nil t)

(defgroup ldd-present nil
  "Configuración personal para presentaciones con Org."
  :group 'org)

(defcustom ldd/present-text-scale 1.6
  "Escala base del texto durante la presentación."
  :type 'float
  :group 'ldd-present)

(defcustom ldd/present-width 110
  "Ancho visual del contenido centrado durante la presentación."
  :type 'integer
  :group 'ldd-present)

(defcustom ldd/present-hide-notes t
  "Si es no-nil, oculta subárboles etiquetados con :notes: al presentar."
  :type 'boolean
  :group 'ldd-present)

(defcustom ldd/present-notes-tag "notes"
  "Tag usado para marcar notas privadas del docente."
  :type 'string
  :group 'ldd-present)

(defvar ldd/present--saved-mode-line nil)
(defvar ldd/present--saved-header-line nil)
(defvar ldd/present--saved-face-remapping nil)

(defun ldd/present--org-present-available-p ()
  "Indica si org-present está disponible."
  (featurep 'org-present))

(defun ldd/present--visual-fill-available-p ()
  "Indica si visual-fill-column está disponible."
  (featurep 'visual-fill-column))

(defun ldd/present-buffer-p ()
  "Indica si el buffer actual está en `org-present-mode'."
  (bound-and-true-p org-present-mode))

(defun ldd/present-prepare-slide ()
  "Muestra el slide actual de forma limpia."
  (when (derived-mode-p 'org-mode)
    (org-overview)
    (org-show-entry)
    (org-show-children)
    (recenter)))

(defun ldd/present-hide-notes ()
  "Oculta headings con el tag configurado en `ldd/present-notes-tag'."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (org-map-entries
       (lambda ()
         (org-fold-hide-subtree))
       (concat "+" ldd/present-notes-tag)))))

(defun ldd/present-show-notes ()
  "Muestra headings con el tag configurado en `ldd/present-notes-tag'."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (org-map-entries
       (lambda ()
         (org-fold-show-subtree))
       (concat "+" ldd/present-notes-tag)))))

(defun ldd/present-toggle-notes ()
  "Alterna visibilidad de notas."
  (interactive)
  (if (y-or-n-p "¿Mostrar notas del docente? ")
      (ldd/present-show-notes)
    (ldd/present-hide-notes)))

(defun ldd/present-display-inline-images ()
  "Muestra imágenes inline si existen."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (org-display-inline-images)))

(defun ldd/present-remove-inline-images ()
  "Oculta imágenes inline."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (org-remove-inline-images)))

(defun ldd/present-refresh ()
  "Refresca la visualización del slide actual."
  (interactive)
  (ldd/present-display-inline-images)
  (ldd/present-prepare-slide))

(defun ldd/present-start ()
  "Hook de entrada a `org-present-mode'."
  (setq ldd/present--saved-mode-line mode-line-format)
  (setq ldd/present--saved-header-line header-line-format)
  (setq ldd/present--saved-face-remapping face-remapping-alist)

  (setq-local face-remapping-alist
              `((default (:height ,ldd/present-text-scale) default)
                (header-line (:height 3.5) variable-pitch)
                (org-document-title (:height 1.8) org-document-title)
                (org-level-1 (:height 1.6) org-level-1)
                (org-level-2 (:height 1.3) org-level-2)
                (org-level-3 (:height 1.2) org-level-3)
                (org-block (:height 1.05) org-block)
                (org-code (:height 1.05) org-code)
                (org-verbatim (:height 1.05) org-verbatim)
                (org-table (:height 1.05) org-table)))

  (setq-local header-line-format " ")
  (setq-local mode-line-format nil)

  (when (fboundp 'display-line-numbers-mode)
    (display-line-numbers-mode -1))

  (visual-line-mode 1)
  (when (fboundp 'variable-pitch-mode)
    (variable-pitch-mode 1))

  (when (ldd/present--visual-fill-available-p)
    (visual-fill-column-mode 1)
    (setq-local visual-fill-column-width ldd/present-width)
    (setq-local visual-fill-column-center-text t))

  (setq-local org-hide-emphasis-markers t)
  (setq-local org-pretty-entities t)

  (ldd/present-display-inline-images)

  (when ldd/present-hide-notes
    (ldd/present-hide-notes))

  (ldd/present-prepare-slide))

(defun ldd/present-end ()
  "Hook de salida de `org-present-mode'."
  (setq-local face-remapping-alist ldd/present--saved-face-remapping)
  (setq-local header-line-format ldd/present--saved-header-line)
  (setq-local mode-line-format ldd/present--saved-mode-line)

  (visual-line-mode -1)
  (when (bound-and-true-p variable-pitch-mode)
    (variable-pitch-mode -1))
  (when (and (ldd/present--visual-fill-available-p)
             (bound-and-true-p visual-fill-column-mode))
    (visual-fill-column-mode -1))

  (ldd/present-remove-inline-images)
  (org-fold-show-all))

(defun ldd/present-start-or-quit ()
  "Inicia o sale de la presentación."
  (interactive)
  (unless (ldd/present--org-present-available-p)
    (user-error "org-present no está instalado"))
  (if (ldd/present-buffer-p)
      (org-present-quit)
    (org-present)))

(defun ldd/present-next ()
  "Avanza al siguiente slide."
  (interactive)
  (unless (ldd/present--org-present-available-p)
    (user-error "org-present no está instalado"))
  (org-present-next))

(defun ldd/present-prev ()
  "Vuelve al slide anterior."
  (interactive)
  (unless (ldd/present--org-present-available-p)
    (user-error "org-present no está instalado"))
  (org-present-prev))

(defun ldd/present-jump-to-top ()
  "Va al principio del documento y refresca."
  (interactive)
  (goto-char (point-min))
  (org-next-visible-heading 1)
  (ldd/present-refresh))

(defun ldd/present-insert-slide ()
  "Inserta un nuevo slide de nivel 1."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Este comando solo funciona en org-mode"))
  (end-of-line)
  (unless (bolp) (insert "\n"))
  (insert "* Título del slide\n- \n")
  (forward-line -1)
  (end-of-line))

(defun ldd/present-insert-subslide ()
  "Inserta un subslide o sección secundaria."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Este comando solo funciona en org-mode"))
  (end-of-line)
  (unless (bolp) (insert "\n"))
  (insert "** Subtema\n- \n")
  (forward-line -1)
  (end-of-line))

(defun ldd/present-insert-teacher-notes ()
  "Inserta un bloque simple de notas del docente."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Este comando solo funciona en org-mode"))
  (end-of-line)
  (unless (bolp) (insert "\n"))
  (insert (format "* Notas del docente :%s:\n- \n" ldd/present-notes-tag))
  (forward-line -1)
  (end-of-line))

(defvar ldd/present-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s") #'ldd/present-start-or-quit)
    (define-key map (kbd "n") #'ldd/present-next)
    (define-key map (kbd "p") #'ldd/present-prev)
    (define-key map (kbd "r") #'ldd/present-refresh)
    (define-key map (kbd "t") #'org-toggle-inline-images)
    (define-key map (kbd "a") #'org-fold-show-all)
    (define-key map (kbd "h") #'ldd/present-hide-notes)
    (define-key map (kbd "m") #'ldd/present-show-notes)
    (define-key map (kbd "j") #'ldd/present-jump-to-top)
    (define-key map (kbd "i") #'ldd/present-insert-slide)
    (define-key map (kbd "u") #'ldd/present-insert-subslide)
    (define-key map (kbd "d") #'ldd/present-insert-teacher-notes)
    map)
  "Mapa prefijo para comandos de presentación.")

(defun ldd/present-setup-default-bindings ()
  "Instala bindings globales recomendados bajo <f8>."
  (interactive)
  (define-key global-map (kbd "<f8>") ldd/present-prefix-map))

(defun ldd/present-setup-org-present-hooks ()
  "Configura hooks de org-present."
  (interactive)
  (add-hook 'org-present-mode-hook #'ldd/present-start)
  (add-hook 'org-present-mode-quit-hook #'ldd/present-end)
  (add-hook 'org-present-after-navigate-functions #'ldd/present-prepare-slide))

(defun ldd/present-setup ()
  "Configuración completa recomendada."
  (interactive)
  (setq org-hide-emphasis-markers t
        org-pretty-entities t
        org-startup-indented t
        org-ellipsis " ▼"
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-image-actual-width nil
        org-hide-leading-stars t
        org-adapt-indentation nil
        org-fontify-quote-and-verse-blocks t)

  (ldd/present-setup-default-bindings)
  (ldd/present-setup-org-present-hooks))

(provide 'ldd-present)
;;; ldd-present.el ends here
