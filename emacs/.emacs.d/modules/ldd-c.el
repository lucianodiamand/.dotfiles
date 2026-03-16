;; -*- lexical-binding: t; -*-

(use-package cc-mode
  :mode ("\\.c\\'" "\\.h\\'")
  :hook (c-mode . lsp))

(use-package clang-format
  :commands (clang-format-buffer clang-format-region))

(provide 'ldd-c)
