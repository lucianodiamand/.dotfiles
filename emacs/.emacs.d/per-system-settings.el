(require 'map) ;; Needed for map-merge

(setq dw/system-settings
      (append
       ;; Put all system-specific settings at the front so that their values are
       ;; found first

       (when (equal system-name "tarvos")
         '((desktop/dpi . 180)
           (emacs/default-face-size . 102)
           (emacs/variable-face-size . 115)
           (emacs/fixed-face-size . 102)))

       '((desktop/dpi . 180)
         (emacs/default-face-size . 110)
         (emacs/variable-face-size . 120)
         (emacs/fixed-face-size . 110))))
