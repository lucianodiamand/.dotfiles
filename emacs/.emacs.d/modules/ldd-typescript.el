;; -*- lexical-binding: t; -*-

(use-package typescript-mode
  :straight t
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp))

(use-package lsp-mode
  :straight t
  :commands lsp
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :custom
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-completion-provider :capf) ;; autocompletado vía company-capf
  (lsp-prefer-flymake nil))       ;; preferir flycheck si está disponible

(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-sideline-enable t)
  (lsp-ui-doc-enable t))

(use-package company
  :straight t
  :hook (after-init . global-company-mode))

(use-package flycheck
  :straight t
  :init (global-flycheck-mode))

(provide 'ldd-typescript)

