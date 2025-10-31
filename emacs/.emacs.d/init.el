;; Disable package.el
(setq package-enable-at-startup nil)

;; Add configuration modules to load path
(add-to-list 'load-path '"~/.emacs.d/modules")

(setq inhibit-startup-message t
      inhibit-splash-screen t
      display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

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

(set-face-attribute 'default nil :font "Hack Nerd Font Mono" :height 110)

;; Zoom por buffer
(global-set-key (kbd "<C-up>")   #'text-scale-increase)
(global-set-key (kbd "<C-down>") #'text-scale-decrease)

;; Reset del zoom
(defun my/text-scale-reset () (interactive) (text-scale-set 0))
(global-set-key (kbd "C-0") #'my/text-scale-reset)
(setq text-scale-mode-step 1.0)

;; Load pertinent modules
(require 'ldd-straight)
(require 'ldd-settings)
(require 'ldd-keys-evil)

(require 'ldd-core)
(require 'ldd-org-roam)
(require 'ldd-pdf-tools)
(require 'ldd-vertico)

(require 'ldd-android)
(require 'ldd-typescript)
(require 'ldd-scheme)
;;(require 'ldd-lua)

(require 'ldd-gpt)

(require 'ldd-mail)
(require 'ldd-org-gtd)
(require 'ldd-present)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
