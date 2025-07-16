;;; ldd-lua.el --- Configuración para desarrollo en Lua -*- lexical-binding: t; -*-

;;; Commentary:
;; Soporte para archivos .lua con lsp, completado y formateo.

;;; Code:

(use-package lua-mode
  :ensure t
  :mode "\\.lua\\'"
  :interpreter "lua"
  :hook (lua-mode . lsp-deferred))

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :config
  (add-to-list 'lsp-language-id-configuration '(lua-mode . "lua"))
  (setq lsp-clients-lua-language-server-bin "lua-language-server") ; Asegurate que esté en tu $PATH
  (setq lsp-clients-lua-language-server-main-location nil)) ; deja que lo autodetecte

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode))

(use-package reformatter
  :ensure t)

(use-package stylua
  :after lua-mode
  :hook (lua-mode . stylua-format-on-save-mode)
  :config
  (reformatter-define stylua-format
    :program "stylua"
    :args '("-")
    :stdin t
    :mode 'lua-mode))

(provide 'ldd-lua)
;;; ldd-lua.el ends here

