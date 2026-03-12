;; -*- lexical-binding: t; -*-

(defun ldd/org-present-prepare-slide ()
  (org-overview)
  (org-show-entry)
  (org-show-children))

(defvar-local ldd/org-present--line-numbers nil)
(defvar-local ldd/org-present--header-line-format nil)
(defvar-local ldd/org-present--face-remapping nil)
(defvar-local ldd/org-present--hide-emphasis nil)
(defvar-local ldd/org-present--image-actual-width nil)
(defvar-local ldd/org-present--line-spacing nil)
(defvar-local ldd/org-present--visual-line-mode nil)
(defvar-local ldd/org-present--org-appear-mode nil)

(defun ldd/org-present-hook ()
  ;; Desactivamos numeros de linea
  (setq-local ldd/org-present--line-numbers (bound-and-true-p display-line-numbers-mode))
  (display-line-numbers-mode -1)

  (setq-local ldd/org-present--face-remapping face-remapping-alist)
  (setq-local face-remapping-alist '((default (:height 1.5) default)
                                     (header-line (:height 4.5) variable-pitch)
                                     (org-document-title (:height 1.75) org-document-title)
                                     ;; These were needed before the Wayland transition
                                     ;; (org-code (:height 1.55) org-code)
                                     ;; (org-verbatim (:height 1.55) org-verbatim)
                                     ;; (org-block (:height 1.1) org-block)
                                     (org-block-begin-line (:height 0.7) org-block)))
  (setq-local ldd/org-present--header-line-format header-line-format)
  (setq header-line-format " ")
  (setq-local ldd/org-present--hide-emphasis org-hide-emphasis-markers)
  (setq-local org-hide-emphasis-markers t)
  (setq-local ldd/org-present--image-actual-width org-image-actual-width)
  (setq-local org-image-actual-width '(800))
  (setq-local ldd/org-present--line-spacing line-spacing)
  (setq-local line-spacing 0.2)
  (setq-local ldd/org-present--visual-line-mode (bound-and-true-p visual-line-mode))
  (visual-line-mode 1)
  (when (fboundp 'org-appear-mode)
    (setq-local ldd/org-present--org-appear-mode (bound-and-true-p org-appear-mode))
    (org-appear-mode -1))
  (org-display-inline-images)
  (ldd/org-present-prepare-slide)
  (when (fboundp 'ldd/kill-panel)
    (ldd/kill-panel)))

(defun ldd/org-present-quit-hook ()
  ;; Reactivamos numeros de linea
  (display-line-numbers-mode (if ldd/org-present--line-numbers 1 -1))

  (setq-local face-remapping-alist ldd/org-present--face-remapping)
  (setq header-line-format ldd/org-present--header-line-format)
  (setq-local org-hide-emphasis-markers ldd/org-present--hide-emphasis)
  (setq-local org-image-actual-width ldd/org-present--image-actual-width)
  (setq-local line-spacing ldd/org-present--line-spacing)
  (visual-line-mode (if ldd/org-present--visual-line-mode 1 -1))
  (org-present-small)
  (org-remove-inline-images)
  (when (fboundp 'org-appear-mode)
    (org-appear-mode (if ldd/org-present--org-appear-mode 1 -1)))
  (when (fboundp 'ldd/start-panel)
    (ldd/start-panel)))

(defun ldd/org-present-next ()
  "Avanza al siguiente slide."
  (interactive)
  (org-present-next)
  (ldd/org-present-prepare-slide)
  (when (fboundp 'live-crafter-add-timestamp)
    (live-crafter-add-timestamp (substring-no-properties (org-get-heading t t t t)))))
(defun ldd/org-present-prev ()
  "Vuelve al slide anterior."
  (interactive)
  (org-present-prev)
  (ldd/org-present-prepare-slide))
(use-package org-present
  :commands org-present
  :hook ((org-present-mode . ldd/org-present-hook)
         (org-present-mode-quit . ldd/org-present-quit-hook))
  :bind (:map org-present-mode-keymap
              ("C-c C-j" . ldd/org-present-next)
              ("C-c C-k" . ldd/org-present-prev)))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c P") #'org-present)
  (define-key org-mode-map (kbd "C-c q") #'org-present-quit))

(provide 'ldd-present)
