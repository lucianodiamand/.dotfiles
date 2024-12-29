;; -*- lexical-binding: t; -*-

;; Set up package.el and use-package

(require 'package)

;; Add MELPA repository
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;; Automatically install packages
(setq use-package-always-ensure t)

;; Never load a package until demanded or triggered
;;(setq use-package-always-defer t)

(provide 'ldd-package)
