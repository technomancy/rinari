;;; rails-script.el --- functions for executing Ruby on Rails scripts

;;; Commentary:

;; Part of Rinari (with pieces taken from emacs-rails)

;;; Code:
(require 'ansi-color)

(defvar rails-default-environment
  "development"
  "the default environment to be used when running rails/scripts")

;;;###autoload
(defun rails-script (arg)
  "Run script/*.  Use a prefix argument to edit command line options."
  (interactive "P")
  (let* ((script (expand-file-name
		  (if (or (equalp ido-mode 'file)
			  (equalp ido-mode 'both))
		      (ido-read-file-name "script: " (concat (rails-root)
							     "/script"))
		    (read-file-name "script: " (concat (rails-root)
						       "/script")))))
	 (command (file-name-nondirectory script)))
    (cond
      ;; console: start inferior ruby process and jump to the buffer
      ((string-equal command "console")
       (rails-script-console arg))
      ;; server: filter ascii colors and dump to a compilation buffer
      ((string-equal command "server")
       (rails-script-server arg))
      ;; generate
      ((string-equal command "generate")
       (rails-script-generate))
      ;; destroy
      ((string-equal command "destroy")
       (rails-script-destroy arg))      
      ;; all others execute and message the output
      (t
       (message "%s"
		(shell-command-to-string script))))))

;; generate and destroy
(defvar rails-script-generators
  '("controller", "integration_test", "mailer", "migration", "model",
    "observer", "plugin", "resource", "scaffold", "session_migration"))

(defun rails-script-generate ()
  "Run script/generate"
  (interactive)
  (let* ((what (completing-read "generate what: " rails-script-generators))
	 (arguments
	  (read-from-minibuffer
	   (format "arguments to script/generate %s: " what))))
    (message "%s"
	     (shell-command-to-string
	      (format "%s/script/generate %s %s" (rails-root) what arguments)))))

(defun rails-script-destroy (arg)
  "Run script/destroy"
  (interactive "P")
  (let* ((what (completing-read "destroy what: " rails-script-generators))
	 (which (read-from-minibuffer
		 (format "destroy which %s: " what)))
	 (arguments (if arg
			(read-from-minibuffer
			 (format "arguments to script/destroy %s %s: " what which))
		      "")))
    (message "%s"
	     (shell-command-to-string
	      (format "%s/script/destroy %s %s %s" (rails-root) what which arguments)))))

;; console
(defun rails-script-console (arg)
  "Run script/console.  Use a prefix argument to edit command line options."
  (interactive "P")
  (let* ((arguments (list
		     (if arg
			 (read-from-minibuffer "arguments to script/console: "
					       rails-default-environment)
		       rails-default-environment)))
	 (buffer (apply 'make-comint
			"console"
			(concat (rails-root) "/script/console")
			nil
			arguments)))
    (pop-to-buffer buffer)
    (inferior-ruby-mode)
    ;; rails/console regexps
    (make-local-variable 'inferior-ruby-first-prompt-pattern)
    (make-local-variable 'inferior-ruby-prompt-pattern)
    (setq inferior-ruby-first-prompt-pattern "^>> "
	  inferior-ruby-prompt-pattern "^>> ")))

;; server
(defvar rails-server-error-regexp
  ;; "\\([^[:space:]]*\\.r[bhtml]+\\):\\([[:digit:]]+\\)"
  "^[[:space:]]+\\([^[:space:]]*\\):\\([[:digit:]]+\\)"
  "regular expression to match errors in the server logs")

(defvar rails-server-compilation-error-regexp-alist
  `((,rails-server-error-regexp 1 2))
  "a version of `compilation-error-regexp-alist' to be used in
  rails logs (should be used with `make-local-variable')")

(defun rails-script-server (arg)
  "Run script/server.  Prefix arg to edit command line options."
  (interactive "P")
  (if (and (get-buffer "*server*")
	   (get-buffer-process "*server*"))
      (error "server is already running")
    (let* ((default-directory (rails-root))
	   (default-arguments (format "-e %s" rails-default-environment))
	   (arguments (split-string
		       (if arg
			   (read-from-minibuffer
			    "arguments to script/server: "
			    default-arguments)
			 default-arguments)
		       " "))
	   (buffer (apply 'make-comint
			  "server"
			  (concat (rails-root) "/script/server")
			  nil
			  arguments)))
      (save-excursion
	(set-buffer buffer)
	;; remove ascii coloring tokens
	(set-process-filter (get-buffer-process buffer)
			    'rails-script-server-insertion-filter)
	;; allow jumping to error messages
	(set (make-variable-buffer-local 'compilation-error-regexp-alist)
	     rails-server-compilation-error-regexp-alist)
	;; \C-c\C-c kills server
	(define-key compilation-minor-mode-map (kbd "C-c C-c") 'comint-interrupt-subjob)
	;; kill process when buffer is killed
	(set (make-variable-buffer-local 'kill-buffer-hook)
	     (lambda ()
	       (let ((proc (get-buffer-process (buffer-name))))
		 (if proc
		     (kill-process proc)))))
	(compilation-minor-mode)
	(message "started script/server %s" (join-string arguments " "))))))

(defun rails-script-server-insertion-filter (proc string)
  (with-current-buffer (process-buffer proc)
    (let ((moving (= (point) (process-mark proc))))
      (save-excursion
	;; Insert the text, advancing the process marker.
	(goto-char (process-mark proc))
	;; translate ansi color codes
	;; (insert (ansi-color-apply string))
	;; ;; remove ansi color codes
	(insert (ansi-color-filter-apply string))
	(set-marker (process-mark proc) (point)))
      (if moving (goto-char (process-mark proc))))))

(provide 'rails-scripts)
;;; rails-scripts.el ends here