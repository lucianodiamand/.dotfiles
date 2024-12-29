;; -*- lexical-binding: t; -*-

;; Appearance

;; Solarized-theme
(unless (package-installed-p 'solarized-theme)
  (package-install 'solarized-theme))

(load-theme 'solarized-dark t)

(provide 'ldd-core)
