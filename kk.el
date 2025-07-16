;; -*- lexical-binding: t; -*-

(use-package lsp-mode
  :hook ((java-mode . lsp-deferred))
  :commands (lsp lsp-deferred)
  :custom
  (lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-java
  :after lsp-mode
  :custom
  ;; Importante: no permitir que intente instalar jdtls
  (lsp-java-server-install-dir nil)
  :config
  (add-hook 'java-mode-hook #'lsp-deferred)

  ;; Extra: si querés compilar con Java 8 pero correr jdtls con Java 21
  (setq lsp-java-configuration-runtimes
        `[(:name "JavaSE-1.8"
                 :path ,(string-trim (shell-command-to-string "nix eval --raw nixpkgs#jdk8.home"))
                 :default t)]))

(use-package company
  :config
  (global-company-mode))

(use-package gradle-mode
  :hook (java-mode . gradle-mode))

(use-package projectile
  :config
  (projectile-mode))

(use-package dap-mode
  :after lsp-mode
  :config
  (dap-auto-configure-mode))

;; Integración con direnv (si usás flakes)
(use-package envrc
  :config
  (envrc-global-mode))

;; Si lsp-java no registra bien el cliente, lo forzamos:
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '(java-mode . "java"))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection "jdtls")
    :activation-fn (lsp-activate-on "java")
    :server-id 'jdtls)))

(provide 'ldd-android)

