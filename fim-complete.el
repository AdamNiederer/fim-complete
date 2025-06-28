;;; fim-complete.el --- Fill-in-the-Middle completion using Ollama  -*- lexical-binding: t; -*-

(require 'json)
(require 'vc-git)

(defgroup fim-complete nil
  "Customizations for FIM completion."
  :group 'completion)

(defface fim-complete-overlay-face
  '((t :inherit shadow))
  "Face used for completion overlay text."
  :group 'fim-complete)

(defcustom fim-complete-model "qwen2.5-coder:32b-base-iq3_M"
  "Model to use for completion."
  :type 'string
  :group 'fim-complete)

(defcustom fim-complete-weak-model "qwen2.5-coder:7b-base-q8_0"
  "Model to use for completion."
  :type 'string
  :group 'fim-complete)

(defcustom fim-complete-url "http://localhost:11434/api/generate"
  "URL to use for completion"
  :type 'string
  :group 'fim-complete)

(defcustom fim-complete-idle-delay 0.2
  "Time in seconds to wait after last command before starting completion."
  :type 'number
  :group 'fim-complete)

(defcustom fim-complete-num-ctx 8192
  "Size of the context window in tokens"
  :type 'number
  :group 'fim-complete)

(defcustom fim-complete-num-predict 128
  "Maximum number of tokens to predict for a completion"
  :type 'number
  :group 'fim-complete)

(defcustom fim-complete--context-files-count 5
  "The maximum number of files placed in the repomap."
  :type 'number
  :group 'fim-complete)

(defvar fim-complete--context-files (make-hash-table :test 'equal)
  "Hashtable of project directories to list of files.")

(defvar fim-complete--overlay nil
  "Overlay displaying current completion.")

(defvar-local fim-complete--post-command-timer nil
  "Timer to run `fim-complete' after idle time.")

(defvar-local fim-complete--fetching nil
  "Whether fim-complete is currently fetching data")

(defconst fim-complete--file-sep "<|file_sep|>"
  "Separator for files in context.")

(defconst fim-complete--fim-prefix "<|fim_prefix|>")
(defconst fim-complete--fim-suffix "<|fim_suffix|>")
(defconst fim-complete--fim-middle "<|fim_middle|>")

;;;###autoload
(defun fim-complete-add-file (&optional file)
  "Add FILE to context files.
With prefix arg, query user for file path."
  (interactive
   (list (or current-prefix-arg
             (read-file-name "Select file to add: "))))
  (let* ((file (or file (buffer-file-name)))
         (root (vc-git-root (file-name-directory file))))
    (when root
      (puthash root
               (thread-last (gethash root fim-complete--context-files '())
                            (cons file)
                            (seq-uniq)
                            (seq-take fim-complete--context-files-count))
               fim-complete--context-files))))

;;;###autoload
(defun fim-complete-remove-file (&optional file)
  "Remove FILE from context files.
With prefix arg, query user for file path."
  (interactive
   (list (or current-prefix-arg
             (read-file-name "Select file to remove: "))))
  (let* ((file (or file (buffer-file-name)))
         (root (vc-git-root (file-name-directory file))))
    (when root
      (puthash root
               (remove file (gethash root fim-complete--context-files '()))
               fim-complete--context-files))))

;;;###autoload
(defun fim-complete-list-files ()
  "List the files in the current project that are included in the context."
  (interactive)
  (let* ((buffer-file (buffer-file-name))
         (root (and buffer-file (vc-git-root (file-name-directory buffer-file)))))
    (if (not root)
        (message "Not in a git project")
      (let ((files (gethash root fim-complete--context-files)))
        (if (not files)
            (message "No files in repomap for project" root)
          (message "%s" root
                   (mapconcat 'identity files "\n")))))))

(defun fim-complete--format-repomap (root)
  "Format repo map for ROOT."
  (let ((files (gethash root fim-complete--context-files))
        (repo-name (file-name-nondirectory (directory-file-name root))))
    (concat
     "<|repo_name|>" repo-name "\n"
     (mapconcat
      (lambda (file)
        (concat fim-complete--file-sep (file-relative-name file root) "\n"
                (with-temp-buffer
                  (insert-file-contents file)
                  (buffer-string))))
      files "\n"))))

(defun fim-complete--format-buffer (&optional with-context)
  "Format current buffer for FIM completion.
If WITH-CONTEXT is non-nil, include the file separator and filename."
  (let* ((buffer-file (buffer-file-name))
         (before-point (buffer-substring-no-properties (point-min) (point)))
         (after-point (buffer-substring-no-properties (point) (point-max)))
         (formatted (concat fim-complete--fim-prefix before-point
                            fim-complete--fim-suffix after-point
                            fim-complete--fim-middle)))
    (if with-context
        (concat fim-complete--file-sep
                (file-name-nondirectory buffer-file) "\n"
                formatted)
      formatted)))

(defun fim-complete--request (prompt model callback)
  "Send PROMPT to Ollama API and call CALLBACK with the response."
  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/json")))
        (url-request-data
         (json-encode
          `(("model" . ,model)
            ("stream" . :json-false)
            ("options" . (("num_predict" . ,fim-complete-num-predict) ("num_ctx" . ,fim-complete-num-ctx)))
            ("prompt" . ,prompt))))
        (url-show-status nil)
        (buffer (current-buffer)))
    (setq fim-complete--fetching t)
    (url-retrieve
     fim-complete-url
     (lambda (status)
       (with-current-buffer buffer
         (setq fim-complete--fetching nil))
       (let ((http-buf (current-buffer)))
         (unwind-protect
             (if-let ((success (null (plist-get status :error))))
                 (progn
                   (goto-char url-http-end-of-headers)
                   (condition-case nil
                       (let* ((json (json-parse-buffer :object-type 'plist))
                              (resp (plist-get json :response))
                              (prompt-eval-count (plist-get json :prompt_eval_count))
                              (eval-count (plist-get json :eval_count)))
                         (when (> (+ prompt-eval-count eval-count) fim-complete-num-ctx)
                           (warn "Token count (%s) exceeded num_ctx (%s)"
                                 (+ prompt-eval-count eval-count)
                                 fim-complete--num-ctx))
                         (funcall callback resp))
                     (error (funcall callback ""))))
               (message "FIM Complete request failed"))
           (when (buffer-live-p http-buf)
             (kill-buffer http-buf))))))))

;;;###autoload
(defun fim-complete (model)
  "Perform Fill-in-the-Middle completion at point."
  (interactive (list fim-complete-model))
  (let* ((buffer-file (buffer-file-name))
         (root (and buffer-file (vc-git-root (file-name-directory buffer-file))))
         resp
         prompt)
    (when buffer-file
      (cond
       (root
        (unless (member buffer-file (gethash root fim-complete--context-files))
          (fim-complete-add-file buffer-file))
        (setq prompt (concat (fim-complete--format-repomap root)
                             (fim-complete--format-buffer t))))
       (t
        ;; No root: use only current buffer
        (setq prompt (fim-complete--format-buffer nil))))
      (when fim-complete--overlay (delete-overlay fim-complete--overlay))
      (setq fim-complete--overlay (make-overlay (point) (point) nil nil nil))
      (overlay-put fim-complete--overlay 'fim-complete t)
      (fim-complete--request
       prompt model
       (lambda (resp)
         (when (and (stringp resp) (> (length resp) 0)
                    (not (string-match-p "\\`[[:space:]\n]*\\'" resp))
                    ;; fim-complete--overlay might be removed by the time we get the response
                    fim-complete--overlay)
           (put-text-property 0 1 'cursor t resp)
           (put-text-property 0 (length resp) 'face 'fim-complete-overlay-face resp)
           (overlay-put fim-complete--overlay 'after-string resp)))))))

(defvar fim-complete-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c RET") 'fim-complete-insert)
    (define-key map (kbd "C-c TAB") 'fim-complete)
    (define-key map (kbd "C-c DEL") 'fim-complete-reject)
    map)
  "Keymap for FIM complete minor mode.")

(defun fim-complete-insert ()
  "Insert current completion."
  (interactive)
  (when (and fim-complete--overlay (overlay-get fim-complete--overlay 'after-string))
    (insert (overlay-get fim-complete--overlay 'after-string))
    (fim-complete-reset-overlay)))

(defun fim-complete-reject ()
  "Clear FIM overlay."
  (interactive)
  (fim-complete-reset-overlay))

(defun fim-complete-insert-or-complete ()
  "Insert the current completion or start a new one."
  (interactive)
  (if (and fim-complete--overlay (overlay-get fim-complete--overlay 'after-string))
      (fim-complete-insert)
    (call-interactively 'fim-complete)))

(defun fim-complete--post-command ()
  "Complete in `post-command-hook' hook."
  (unless (memq this-command '(fim-complete fim-complete-insert))
    (fim-complete-reset-overlay))

  (when fim-complete--post-command-timer
    (cancel-timer fim-complete--post-command-timer))

  (unless fim-complete--fetching
    (setq fim-complete--post-command-timer
          (run-with-idle-timer
           fim-complete-idle-delay
           nil
           (lambda ()
             (when (and (fim-complete-minor-mode-p) (not (minibufferp)))
               (fim-complete fim-complete-weak-model)))))))

(defun fim-complete-reset-overlay ()
  "Clear FIM overlay."
  (when fim-complete--overlay
    (delete-overlay fim-complete--overlay)
    (setq fim-complete--overlay nil)))

;;;###autoload
(defun fim-complete-minor-mode-p ()
  "Return non-nil if `fim-complete-minor-mode' is enabled in current buffer."
  fim-complete-minor-mode)

;;;###autoload
(define-minor-mode fim-complete-minor-mode
  "Toggle FIM Complete mode."
  :lighter " FIM"
  :global nil
  (if fim-complete-minor-mode
      (progn
        (add-hook 'post-command-hook 'fim-complete--post-command nil t)
        (add-hook 'kill-buffer-hook
                  (lambda ()
                    (when fim-complete--post-command-timer
                      (cancel-timer fim-complete--post-command-timer)
                      (setq fim-complete--post-command-timer nil))
                    (remove-hook 'post-command-hook 'fim-complete--post-command t))
                  nil t))
    (remove-hook 'post-command-hook 'fim-complete--post-command t)
    (when fim-complete--post-command-timer
      (cancel-timer fim-complete--post-command-timer)
      (setq fim-complete--post-command-timer nil))
    (fim-complete-reset-overlay)))

(provide 'fim-complete)
;;; fim-complete.el ends here
