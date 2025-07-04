;; -*- lexical-binding: t; -*-

;; --- Set variables BEFORE evil is loaded ---
(setq evil-want-integration t
      evil-want-keybinding nil
      evil-want-C-u-scroll t)

;; -- Evil Mode configuration -----
(use-package evil
  :demand t
  :init
  ;; Pre-load configuration
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :config
  (evil-commentary-mode))

(provide 'ldd-keys-evil)

