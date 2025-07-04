;; -*- lexical-binding: t; -*-

(use-package geiser
  :straight t
  :hook ((scheme-mode . geiser-mode)
         (geiser-mode . company-mode)
         (geiser-mode . flycheck-mode))
  :config
  (setq geiser-active-implementations '(guile)))

(use-package geiser-guile
  :straight t
  :after geiser
  :config
  (setq geiser-guile-binary "guile"))

(use-package company
  :straight t
  :defer t
  :config
  (global-company-mode))

(use-package flycheck
  :straight t
  :defer t
  :config
  (global-flycheck-mode))

(provide 'ldd-scheme)
