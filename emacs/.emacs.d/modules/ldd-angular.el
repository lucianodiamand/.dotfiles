
;;; ldd-angular.el --- Angular dev setup -*- lexical-binding: t; -*-

(require 'use-package)

;; =========================
;;  Tree-sitter / major modes
;; =========================

;; Preferir los modos *-ts-mode de Emacs 30
(when (fboundp 'treesit-available-p)
  (when (treesit-available-p)
    ;; Remap clásico -> tree-sitter
    (add-to-list 'major-mode-remap-alist
                 '(typescript-mode . typescript-ts-mode))
    (add-to-list 'major-mode-remap-alist
                 '(css-mode . css-ts-mode))
    (add-to-list 'major-mode-remap-alist
                 '(js-mode . js-ts-mode))
    (add-to-list 'major-mode-remap-alist
                 '(html-mode . html-ts-mode))))

;; =========================
;;  LSP: lsp-mode + lsp-ui
;; =========================

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((typescript-ts-mode . lsp-deferred)
         (typescript-mode    . lsp-deferred)
         (web-mode           . lsp-deferred))
  :init
  (setq lsp-keymap-prefix "C-c l"
        lsp-headerline-breadcrumb-enable t
        lsp-enable-snippet t
        lsp-enable-indentation nil   ;; dejamos la indentación al modo / prettier
        lsp-enable-on-type-formatting nil)
  :config
  ;; Un poco menos de spam
  (setq lsp-log-io nil))

(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :init
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-position 'at-point
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-code-actions t))

;; =========================
;;  Autocompletado & errores
;; =========================

(use-package company
  :hook (after-init . global-company-mode)
  :init
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.1))

(use-package flycheck
  :hook (lsp-mode . flycheck-mode))

;; =========================
;;  TypeScript
;; =========================

;; Si por alguna razón no hay tree-sitter, caer a typescript-mode clásico
(use-package typescript-mode
  :mode ("\\.ts\\'" . typescript-mode)
  :init
  (setq typescript-indent-level 2))

;; =========================
;;  Angular templates: web-mode
;; =========================

(use-package web-mode
  :mode (("\\.html?\\'"          . web-mode)
         ("\\.component.html\\'" . web-mode))
  :init
  (setq web-mode-enable-auto-closing t
        web-mode-enable-auto-quoting t
        web-mode-enable-current-element-highlight t)
  :config
  ;; Decirle que los .component.html son Angular
  (add-to-list 'web-mode-engines-alist
               '("angular" . "\\.component.html\\'")))

;; =========================
;;  SCSS / CSS
;; =========================

(use-package scss-mode
  :mode ("\\.scss\\'" . scss-mode)
  :init
  (setq scss-compile-at-save nil))

;; css-mode ya viene en Emacs, sólo aseguramos la asociación:
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))

;; =========================
;;  HTML helpers (emmet)
;; =========================

(use-package emmet-mode
  :hook ((web-mode . emmet-mode)
         (html-mode . emmet-mode))
  :init
  (setq emmet-expand-jsx-className? t))

;; =========================
;;  Formateo con Prettier (apheleia)
;; =========================

(use-package apheleia
  :config
  ;; Usar el prettier del proyecto vía npx
  (setf (alist-get 'prettier apheleia-formatters)
        '("npx" "prettier" "--stdin-filepath" filepath))
  (dolist (entry '((typescript-ts-mode . prettier)
                   (typescript-mode    . prettier)
                   (web-mode           . prettier)))
    (add-to-list 'apheleia-mode-alist entry))
  (apheleia-global-mode +1))

;; =========================
;;  Angular Language Service
;; =========================
;; lsp-mode ya trae cliente angular; con el npm global suele alcanzar.
;; Si quisieras tunear la ruta, podrías tocar:
;;
;; (setq lsp-clients-angular-language-server-command
;;       '("node"
;;         "/ruta/a/@angular/language-server"
;;         "--stdio"
;;         "--tsProbeLocations" "/ruta/a/node_modules"
;;         "--ngProbeLocations" "/ruta/a/node_modules"))

(provide 'ldd-angular)
;;; ldd-angular.el ends here

