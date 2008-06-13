;;; rails-script.el --- functions for executing Ruby on Rails scripts

;;; Commentary:

;; Part of Rinari (with pieces taken from emacs-rails)

;;; Todo:

;; - some scripts require arguments so shouldn't need the prefix
;;   argument, these scripts (generators, etc...) should for review of
;;   the arguments
;;   

;;; Code:
(require 'ansi-color)

(defvar rails-default-environment
  "eschulte"
  "the default environment to be used when running rails/scripts")

;; TODO:
;; - should allow to edit options with a prefix argument
;;;###autoload
(defun rails-script (arg)
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
      ;; all others execute and message the output
      (t
       (message "%s"
		(shell-command-to-string script))))))

;; console
(defun rails-script-console (arg)
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
  (interactive "P")
  (let* ((default-directory (rails-root))
	 (default-arguments (format "-e %s" rails-default-environment))
	 (arguments (split-string
		     (if arg
			 (read-from-minibuffer
			  "arguments to script/server: "
			  default-arguments)
		       default-arguments)
		     " "))
	 (mes (message "dir %s" default-directory))
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
      ;; simple kill server (not ideal but \C-c\C-c is taken)
      (local-set-key "\C-x\C-c" 'comint-interrupt-subjob)
      (compilation-minor-mode)
      ;; TODO: add something to kill the process and filter when the buffer is killed
      (message "started script/server %s" (join-string arguments " ")))))

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