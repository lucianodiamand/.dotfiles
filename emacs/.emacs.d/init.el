;; Override some of the defaults
;; Disable start up logo

;; Add configuration modules to load path
(add-to-list 'load-path '"~/.emacs.d/modules")

(setq inhibit-startup-message t
      inhibit-splash-screen t
      display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

;; Enable the fill-column indicator globally
(global-display-fill-column-indicator-mode t)
(setq-default fill-column 80)

;; Customize the appearance to use a block character
(set-face-attribute 'fill-column-indicator nil
                    :foreground "gray"
                    :background nil
                    :family "monospace")
                    ;;:inherit nil) ;; Prevent inheriting other styles

;; Custom function to use a block character for the column indicator
(defun my/set-fill-column-indicator-block-character ()
  "Change the fill-column indicator to use a block character."
  (setq display-fill-column-indicator-character ?█))  ; Set the block character

;; Apply the custom character
(add-hook 'display-fill-column-indicator-mode-hook 'my/set-fill-column-indicator-block-character)

;; Disable visible scroolbar
(scroll-bar-mode -1)
;; Disable the toolbar
(tool-bar-mode -1)
;; Disable tooltips
(tooltip-mode -1)
;; Give some breathing room
(set-fringe-mode 10)

;; Disable the menu bar
(menu-bar-mode -1)

;;(transient-mark-mode t)
;;(setq yank-interactive nil)

;; Set up the visible bell
(setq visible-bell t)

;; Remove windows frame
(add-to-list 'default-frame-alist '(undecorated . t))

(set-face-attribute 'default nil :font "Hack Nerd Font Mono" :height 140)

;; Load pertinent modules
(require 'ldd-package)
(require 'ldd-settings)
(require 'ldd-keys-evil)

(require 'ldd-core)
(require 'ldd-auth)
(require 'ldd-mail)
(require 'ldd-present)
(require 'ldd-gpt)

(require 'epg)

(setq backup-directory-alist `(("." . ,(expand-file-name ".tmp/backups/"
                                                         user-emacs-directory))))

(defun efs/display-startup-time()
  (message "Emacs loaded in %s with %d garbage collections"
           (format "%.2f seconds"
                   (float-time
                   (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)


(use-package marginalia
             :init
             (marginalia-mode))

(setq frame-title-format
      '(""
        "%b"
        (:eval
         (let ((project-name (projectile-project-name)))
           (if (and project-name (not (string= project-name "-")))
               (if (buffer-modified-p)
                   (format " ◉ %s - Emacs" project-name)
                 (format "  ●  %s - Emacs" project-name))
             "")))))

;; org-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'org)

(use-package org-bullets
             :ensure t
             :init (add-hook 'org-mode-hook 'org-bullets-mode))

(use-package orgtbl-aggregate
  :ensure t
  :config
  ;; No additional configuration is required; it works with Org tables
  )

;; agenda ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-agenda-files (quote ("~/dev/org")))

;; Disable keys in org-mode
;;   C-c [
;;   C-c ]
;;   C-c ;
;;   C-c C-x C-q  cancelling the clock (we never want this)
(add-hook 'org-mode-hook
          '(lambda ()
             ;; Undefine C-c [ and C-c ] since this breaks my
             ;; org-agenda files when directories are include it
             ;; expands the files in the directories individually
             (org-defkey org-mode-map "\C-c[" 'undefined)
             (org-defkey org-mode-map "\C-c]" 'undefined)
             (org-defkey org-mode-map "\C-c;" 'undefined)
             (org-defkey org-mode-map "\C-c\C-x\C-q" 'undefined))
          'append)

(define-key global-map "\C-ca" 'org-agenda)

(setq org-todo-keyword-faces
     (quote (("TODO" :foreground "red" :weight bold)
             ("NEXT" :foreground "blue" :weight bold)
             ("DONE" :foreground "forest green" :weight bold)
             ("WAITING" :foreground "orange" :weight bold)
             ("HOLD" :foreground "magenta" :weight bold)
             ("CANCELLED" :foreground "forest green" :weight bold)
             ("MEETING" :foreground "forest green" :weight bold)
             ("PHONE" :foreground "forest green" :weight bold))))

(setq calendar-date-stype 'european)
(setq calendar-week-start-day 1)

;; org-gcal ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-gcal-client-id ""
      org-gcal-client-secret ""
      org-gcal-fetch-file-alist '(("lucianodiamand@gmail.com" . "~/dev/org/personal-gmail.org")))

;; vertico ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package vertico
  :ensure t
  :init
  (vertico-mode))

;; Install org-present if need
;;(unless (package-installed-p 'org-present)
;;  (package-install 'org-present))

;; Install visual-fill-column
;;(unless (package-installed-p 'visual-fill-column)
;;  (package-install 'visual-fill-column))

;; Configure fill width
;;(setq visual-fill-column-width 110
;;     visual-fill-column-center-text t)

;;(defun ldd/org-present-start ()
;;  ;; Tweak font sizes
;;  (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
;;                                     (header-line (:height 4.0) variable-pitch)
;;                                     (org-docment-title (:height 1.75) org-document-title)
;;                                     (org-code (:height 1.55) org-code)
;;                                     (org-verbatim (:height 1.55) org-verbatim)
;;                                     (org-block (:height 1.25) org-block)
;;                                     (org-block-begin-line (:height 0.7) org-block)))
;;
;;  ;; Set a blank header line string to create blank space at the top
;;  (setq header-line-format " ")
;;
;;  ;; Center the presentation and wrap lines
;;  (visual-fill-column-mode 1)
;;  (visual-line-mode 1))
;;
;;(defun ldd/org-present-end ()
;;  ;; Reset font customizations
;;  (setq-local face-remapping-alist '((default variable-pitch default)))
;;
;;  (setq header-line-format nil)
;;
;;  ;; Stop centering the document
;;  (visual-fill-column-mode 0)
;;  (visual-line-mode 0))
;;
;;;; Register hooks with org-present
;;(add-hook 'org-present-mode-hook 'ldd/org-present-start)
;;(add-hook 'org-present-mode-quit-hook 'ldd/org-present-end)

;; Load org-faces to make sure we can set appropiate faces
(require 'org-faces)

;; Hide emphasis markers on formatted text
(setq org-hide-emphasis-markers t)

;; Resize Org headings
(dolist (face '((org-level-1 . 1.2)
                (org-level-2 . 1.1)
                (org-level-3 . 1.05)
                (org-level-4 . 1.0)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :font "Hack Nerd Font Mono" :weight 'medium :height (cdr face)))

;; Make the document title a bit bigger
(set-face-attribute 'org-document-title nil :font "Hack Nerd Font Mono" :weight 'bold :height 1.3)

;; Make sure certain org faces use fixed-pitch face when variable-pitch-mode is on
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-table nil :inherit 'fixed-pitch)
(set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

;;(defun ldd/org-present-prepare-slide (buffer-name heading)
;;  ;; Show only top-level headlines
;;  (org-overview)
;;
;;  ;; Unfold the current entry
;;  (org-show-entry)
;;
;;  ;; Show only direct subheadings of the slide but don't expand them
;;  (org-show-children))
;;
;;(add-hook 'org-present-after-navigate-functions 'ldd/org-present-prepare-slide)

(use-package org)
;;(use-package command-log-mode)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)))

(require 'auth-source)
(setq auth-source '((:source "~/.authinfo")))

;; Languages ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Lua
(use-package lua-mode
  :mode "\\.lua\\'")

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (setq treesit-auto-langs '(javascript typescript tsx css html))
  (treesit-auto-add-to-auto-mode-alist '(javascript typescript tsx css html))
  (global-treesit-auto-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((prog-mode . lsp-deferred)
         (lsp-mode . lsp-enabled-which-key-integration))
  :custom
  (read-process-output-max (* 1024 1024))
  :init
  (setq lsp-completion-provider :none)
  (setq lsp-keymap-prefix "C-c")
  (setq lsp-diagnostics-provider :flycheck))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0)
  (corfu-popupinfo-delay '(0.5 . 0.2))
  (corfu-preview-current 'insert)
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)
  :bind (:map corfu-map
              ("TAB"        . corfu-next)
              ([tab]        . corfu-next)
              ("S-TAB"      . corfu-previous)
              ([backtab]    . corfu-previous)
              ("S-<return>" . corfu-insert)
              ("RET"        . corfu-insert))
  :init
  (global-corfu-mode)
  (corfu-history-mode))

(use-package nerd-icons-corfu
  :after corfu
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package flycheck
  :hook (lsp-mode . flycheck-mode)
  :bind (:map flycheck-mode-map
              ("M-n" . flycheck-previous-error)
              ("M-p" . flycheck-next-error))
  :custom (flycheck-display-errors-delay .3))

(use-package apheleia
  :hook (prog-mode . apheleia-mode)
  :config
  (setf (alist-get 'prettier apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath)))

(use-package projectile)
(use-package yasnippet
  :config
  (yas-global-mode))
(use-package hydra)
(use-package company)

;; which key ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package which-key
  :config (which-key-mode))

(use-package lsp-java
  :config
  (add-hook 'java-mode-hook 'lsp))
(use-package dap-mode
  :after
  lsp-mode
  :config (dap-auto-configure-mode))
(use-package dap-java
  :ensure nil)
(use-package helm-lsp)
(use-package helm
  :config
  (helm-mode))
(use-package lsp-treemacs)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-minibuffer-history-key "M-p")
 '(package-selected-packages
   '(gptel apheleia flycheck nerd-icons-corfu corfu lsp-ui lsp-mode treesit-auto lua-mode command-log-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

