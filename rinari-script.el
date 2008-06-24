;;; rinari-script.el --- functions for executing Ruby on Rails scripts

;;; Commentary:

;; Part of Rinari (with pieces taken from emacs-rails)

;;; Code:
(require 'ansi-color)

;; console
(defun rinari-console (&optional arg)
  "Run script/console.  Use a prefix argument to edit command line options."
  (interactive "P")
  (let* ((script (concat (rinari-root) "script/console"))
	 (command (if arg
		      (read-string "Run Ruby: " (concat script " "))
		    script)))
    (run-ruby command)
    (save-excursion (pop-to-buffer "*ruby*")
      (set (make-local-variable 'inferior-ruby-first-prompt-pattern) "^>> ")
      (set (make-local-variable 'inferior-ruby-prompt-pattern) "^>> "))))

(defun rinari-server ()
  "Run script/server."
  (interactive)
  (rinari-run-w/compilation (concat (rails-root) "/script/server")))

;; compilation
(defvar rinari-compilation-error-regexp
  "^[[:space:]]*\\[?\\([^[:space:]]*\\):\\([[:digit:]]+\\)"
  "regular expression to match errors in ruby process output")

(defvar rinari-compilation-error-regexp-alist
  `((,rinari-compilation-error-regexp 1 2))
  "a version of `compilation-error-regexp-alist' to be used in
  rails logs (should be used with `make-local-variable')")

(defun rinari-run-w/compilation (cmd)
  "Run a ruby process dumping output to a ruby compilation buffer."
  (interactive "FRuby Comand: ")
  (let ((default-directory (rinari-root))
	(name (file-name-nondirectory cmd))
	(cmdlist (ruby-args-to-list (expand-file-name cmd))))
    (unless (comint-check-proc (format "*%s*" name))
      (let* ((buffer (apply 'make-comint name "ruby" nil cmdlist))
	     (proc (get-buffer-process buffer)))
	(set-buffer buffer)
	(set-process-sentinel proc 'rinari-compilation-sentinel)
	(set-process-filter proc 'rinari-compilation-insertion-filter)
	(set (make-variable-buffer-local 'compilation-error-regexp-alist)
	     rinari-compilation-error-regexp-alist)
	(define-key compilation-minor-mode-map (kbd "C-c C-c") 'comint-interrupt-subjob)
	(set (make-variable-buffer-local 'kill-buffer-hook)
	     (lambda ()
	       (let ((orphan-proc (get-buffer-process (buffer-name))))
		 (if orphan-proc
		     (kill-process orphan-proc)))))
	(compilation-minor-mode)
	(rinari-minor-mode)))
    (pop-to-buffer (format "*%s*" name))))

(defun rinari-compilation-insertion-filter (proc string)
  "Insert text to buffer stripping ansi color codes"
  (with-current-buffer (process-buffer proc)
    (let ((moving (= (point) (process-mark proc))))
      (save-excursion
	(goto-char (process-mark proc))
	(insert (ansi-color-filter-apply string))
	(set-marker (process-mark proc) (point)))
      (if moving (goto-char (process-mark proc))))))

(defun rinari-compilation-sentinel (proc msg)
  "Notify to changes in process state"
  (message "%s - %s" proc (replace-regexp-in-string "\n" "" msg)))

(defun rinari-compilation-previous-error-group ()
  "Jump to the start of the previous error group in the current
compilation buffer."
  (interactive)
  (compilation-previous-error 1)
  (while (string-match rinari-compilation-error-regexp (thing-at-point 'line))
    (forward-line -1))
  (forward-line 1) (recenter))

(defun rinari-compilation-next-error-group ()
  "Jump to the start of the previous error group in the current
compilation buffer."
  (interactive)
  (while (string-match rinari-compilation-error-regexp (thing-at-point 'line))
    (forward-line 1))
  (compilation-next-error 1) (recenter))

(provide 'rinari-script)
;;; rinari-script.el ends here