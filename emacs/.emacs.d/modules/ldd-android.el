;; -*- lexical-binding: t; -*-

;; LSP para Java
(use-package lsp-mode
  :hook ((java-mode . lsp))
  :commands lsp)

(use-package lsp-java
  :after lsp-mode
  :config
  (add-hook 'java-mode-hook #'lsp))

(use-package company
  :config
  (global-company-mode))

(use-package gradle-mode
  :hook (java-mode . gradle-mode))

(use-package projectile
  :config
  (projectile-mode))

;; Opcional: DAP (debugging)
(use-package dap-mode
  :after lsp-mode
  :config
  (dap-auto-configure-mode))

(provide 'ldd-android)

