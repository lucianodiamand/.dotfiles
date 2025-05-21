;; -*- lexical-binding: t; -*-

(use-package gptel
             :ensure t
             :config
             (setq auth-sources '("~/.authinfo"))
             (setq gptel-api-key (auth-source-pick-first-password :host "api.openai.com"))
             ;;(setq gptel-model "gpt-3.5-turbo")
             (setq gptel-default-model "gpt-3.5-turbo")
             (setq gptel-max-tokens 1000)
             (setq gptel-debug t)
             (setq gptel-retry-on-error t)
             (setq gptel-default-mode 'org-mode)
             (setq gptel-retry-delay 5)
             ;;(setq gptel-backend "openai")
             (setq gptel-history-file "~/gptel-history.org")
             (setq gptel-backends
                   (list
                     (gptel-make-openai "gpt-4o-mini"
                                        :key gptel-api-key
                                        :models '("gpt-4o-mini"))
                     (gptel-make-openai "gpt-4"
                                        :key gptel-api-key
                                        :models '("gpt-4"))
                     (gptel-make-openai "gpt-3.5"
                                        :key gptel-api-key
                                        :models '("gpt-3.5-turbo"))))

             (defun gptel-get-backend (name)
                    "Buscar un backend por nombre."
                    (seq-find (lambda (b) (string= name (gptel-backend-name b))) gptel-backends))

             (defun my-gptel-insert-response-in-org (prompt response)
               "Insertar la conversacion en un archivo org-mode."
               (with-current-buffer (find-file-noselect gptel-history-file)
                                    (goto-char (point-max))
                                    (insert (format "\n* Pregunta (%s)\n:PROPERTIES:\n:fecha: %s\n:modelo: %s\n:END:\n"
                                                    (format-time-string "%Y-%m-%d %H:%M")
                                                    (format-time-string "[%Y-%m-%d %a %H:%M]")
                                                    gptel-default-model))
                                    (insert "\n#+BEGIN_QUOTE\n")
                                    (insert (string-trim prompt))
                                    (insert "\n#+END_QUOTE\n\n")
                                    (insert "** Respuesta\n\n#+BEGIN_SRC text\n")
                                    (insert (string-trim response))
                                    (insert "\n#+END_SRC\n")
                                    (save-buffer)))
             (defun my-gptel-send-prompt ()
               "Enviar un prompt a GPT y guardar automaticamente en org-mode."
               (interactive)
               (let* ((prompt (read-string "Tu pregunta: "))
                      (model (completing-read "Modelo: " '("gpt-4o-mini" "gpt-4" "gpt-3.5" "llama3" "mixtral") nil t nil nil gptel-default-model)))
                 (setq gptel-default-model model)
                 (let ((gptel-backend (gptel-get-backend model)))
                   (gptel-request
                     prompt
                     :stream t
                     :callback (lambda (_meta response)
                                 (message "Respuesta recibida")
                                 (let* ((choices (plist-get response :choices))
                                        (first-choice (if (vectorp choices) (aref choices 0) (car choices))) (text (plist-get (plist-get first-choice :message) :content)))
                                   (my-gptel-insert-response-in-org prompt text))))))))

(provide 'ldd-gpt)
