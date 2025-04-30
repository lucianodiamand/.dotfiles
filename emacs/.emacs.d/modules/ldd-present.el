;; -*- lexical-binding: t; -*-

(defun ldd/org-present-prepare-slide ()
  (org-overview)
  (org-show-entry)
  (org-show-children))

(defun ldd/org-present-hook ()
  ;; Desactivamos numeros de linea
  (display-line-numbers-mode -1)

  (setq-local face-remapping-alist '((default (:height 1.5) default)
                                     (header-line (:height 4.5) variable-pitch)
                                     (org-document-title (:height 1.75) org-document-title)
                                     ;; These were needed before the Wayland transition
                                     ;; (org-code (:height 1.55) org-code)
                                     ;; (org-verbatim (:height 1.55) org-verbatim)
                                     ;; (org-block (:height 1.1) org-block)
                                     (org-block-begin-line (:height 0.7) org-block)))
  (setq header-line-format " ")
  (org-appear-mode -1)
  (org-display-inline-images)
  (ldd/org-present-prepare-slide)
  (when (fboundp 'ldd/kill-panel)
    (ldd/kill-panel)))

(defun ldd/org-present-quit-hook ()
  ;; Reactivamos numeros de linea
  (display-line-numbers-mode t)

  (setq-local face-remapping-alist nil)
  (setq header-line-format nil)
  (org-present-small)
  (org-remove-inline-images)
  (org-appear-mode 1)
  (when (fboundp 'ldd/start-panel)
    (ldd/start-panel)))

(defun ldd/org-present-prev ()
  (interactive)
  (org-present-prev)
  (ldd/org-present-prepare-slide))

(defun ldd/org-present-next ()
  (interactive)
  (org-present-next)
  (ldd/org-present-prepare-slide)
  (when (fboundp 'live-crafter-add-timestamp)
    (live-crafter-add-timestamp (substring-no-properties (org-get-heading t t t t)))))

(use-package org-present
  :commands org-present
  :hook ((org-present-mode . ldd/org-present-hook)
         (org-present-mode-quit . ldd/org-present-quit-hook))
  :bind (:map org-present-mode-keymap
              ("C-c C-j" . ldd/org-present-next)
              ("C-c C-k" . ldd/org-present-prev)))

(provide 'ldd-present)
