;;; rails-script.el --- functions for executing Ruby on Rails scripts

;;; Commentary:

;; Part of Rinari (with pieces taken from emacs-rails)

;;; Code:
(require 'ansi-color)

;; console
(defun rails-script-console (&optional arg)
  "Run script/console.  Use a prefix argument to edit command line options."
  (interactive "P")
  (let* ((script (concat (rails-root) "script/console"))
	 (command (if arg
		      (read-string "Run Ruby: " (concat script " "))
		    script)))
    (run-ruby command)
    (save-excursion (pop-to-buffer "*ruby*")
      (set (make-local-variable 'inferior-ruby-first-prompt-pattern) "^>> ")
      (set (make-local-variable 'inferior-ruby-prompt-pattern) "^>> "))))

;; compilation
(defvar rails-compilation-error-regexp
  "^[[:space:]]*\\[?\\([^[:space:]]*\\):\\([[:digit:]]+\\)"
  "regular expression to match errors in ruby process output")

(defvar rails-compilation-error-regexp-alist
  `((,rails-compilation-error-regexp 1 2))
  "a version of `compilation-error-regexp-alist' to be used in
  rails logs (should be used with `make-local-variable')")

(defun rails-run-w/compilation (cmd)
  "Run a ruby process dumping output to a ruby compilation buffer."
  (interactive "FRuby Comand: ")
  (let ((default-directory (rails-root))
	(name (file-name-nondirectory cmd))
	(cmdlist (ruby-args-to-list (expand-file-name cmd))))
    (unless (comint-check-proc (format "*%s*" name))
      (let* ((buffer (apply 'make-comint name "ruby" nil cmdlist))
	     (proc (get-buffer-process buffer)))
	(set-buffer buffer)
	(set-process-sentinel proc 'rails-compilation-sentinel)
	(set-process-filter proc 'rails-compilation-insertion-filter)
	(set (make-variable-buffer-local 'compilation-error-regexp-alist)
	     rails-compilation-error-regexp-alist)
	(define-key compilation-minor-mode-map (kbd "C-c C-c") 'comint-interrupt-subjob)
	(set (make-variable-buffer-local 'kill-buffer-hook)
	     (lambda ()
	       (let ((orphan-proc (get-buffer-process (buffer-name))))
		 (if orphan-proc
		     (kill-process orphan-proc)))))
	(compilation-minor-mode)
	(rinari-minor-mode)))
    (pop-to-buffer (format "*%s*" name))))

(defun rails-compilation-insertion-filter (proc string)
  "Insert text to buffer stripping ansi color codes"
  (with-current-buffer (process-buffer proc)
    (let ((moving (= (point) (process-mark proc))))
      (save-excursion
	(goto-char (process-mark proc))
	(insert (ansi-color-filter-apply string))
	(set-marker (process-mark proc) (point)))
      (if moving (goto-char (process-mark proc))))))

(defun rails-compilation-sentinel (proc msg)
  "Notify to changes in process state"
  (message "%s - %s" proc (replace-regexp-in-string "\n" "" msg)))

(defun rails-compilation-previous-error-group ()
  "Jump to the start of the previous error group in the current
compilation buffer."
  (interactive)
  (compilation-previous-error 1)
  (while (string-match rails-compilation-error-regexp (thing-at-point 'line))
    (forward-line -1))
  (forward-line 1) (recenter))

(defun rails-compilation-next-error-group ()
  "Jump to the start of the previous error group in the current
compilation buffer."
  (interactive)
  (while (string-match rails-compilation-error-regexp (thing-at-point 'line))
    (forward-line 1))
  (compilation-next-error 1) (recenter))

(provide 'rails-script)
;;; rails-script.el ends here