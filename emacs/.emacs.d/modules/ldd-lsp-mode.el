;; -*- lexical-binding: t; -*-

(use-package lsp-mode
             :ensure t
             :hook (java-mode . lsp)
             :commands lsp
             :config
             (setq lsp-java-server-install-dir "/usr/share/java/jdtls/"))

(provide 'ldd-gpt)
