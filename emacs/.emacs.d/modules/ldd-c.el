;; -*- lexical-binding: t; -*-

(use-package lsp-mode
  :custom
  (lsp-enable-suggest-server-download nil)
  (lsp-completion-provider :capf))

(use-package cc-mode
  :ensure nil
  :hook (c-mode . lsp))

(with-eval-after-load 'company
  (setq company-backends
        '(company-capf company-files company-dabbrev-code company-dabbrev)))

(use-package clang-format
  :commands (clang-format-buffer clang-format-region))

(provide 'ldd-c)
