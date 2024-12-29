;; -*- lexical-binding: t; -*-

(defun ldd/load-system-settings ()
  (interactive)
  (load-file "~/.emacs.d/per-system-settings.el"))

(defun ldd/system-settings-get (setting)
  (alist-get setting ldd/system-settings))

;; Load settings for the first time
(ldd/load-system-settings)


(provide 'ldd-settings)
