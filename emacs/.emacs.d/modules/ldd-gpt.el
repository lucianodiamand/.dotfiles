;; -*- lexical-binding: t; -*-

(use-package gptel
              :straight t
              :config
              (setq auth-sources '("~/.authinfo"))
              (setq gptel-api-key (auth-source-pick-first-password :host "api.openai.com"))
              (setq gptel-model "gpt-3.5-turbo")
              (setq gptel-max-tokens 1000)
              (setq gptel-debug t)
              (setq gptel-retry-on-error t)
              (setq gptel-retry-delay 5))

(provide 'ldd-gpt)
