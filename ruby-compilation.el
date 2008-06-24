;;; ruby-compilation.el --- run a ruby process in a compilation buffer

;; Eric Schulte

;;; Code:
(require 'ansi-color)

(defvar ruby-compilation-error-regexp
  "^[[:space:]]*\\[?\\([^[:space:]]*\\):\\([[:digit:]]+\\)"
  "regular expression to match errors in ruby process output")

(defvar ruby-compilation-error-regexp-alist
  `((,ruby-compilation-error-regexp 1 2))
  "a version of `compilation-error-regexp-alist' to be used in
  rails logs (should be used with `make-local-variable')")

(defun ruby-run-w/compilation (cmd)
  "Run a ruby process dumping output to a ruby compilation buffer."
  (interactive "FRuby Comand: ")
  (let ((default-directory (ruby-root))
	(name (file-name-nondirectory cmd))
	(cmdlist (ruby-args-to-list (expand-file-name cmd))))
    (unless (comint-check-proc (format "*%s*" name))
      (let* ((buffer (apply 'make-comint name "ruby" nil cmdlist))
	     (proc (get-buffer-process buffer)))
	(set-buffer buffer)
	(set-process-sentinel proc 'ruby-compilation-sentinel)
	(set-process-filter proc 'ruby-compilation-insertion-filter)
	(set (make-variable-buffer-local 'compilation-error-regexp-alist)
	     ruby-compilation-error-regexp-alist)
	(define-key compilation-minor-mode-map (kbd "C-c C-c") 'comint-interrupt-subjob)
	(set (make-variable-buffer-local 'kill-buffer-hook)
	     (lambda ()
	       (let ((orphan-proc (get-buffer-process (buffer-name))))
		 (if orphan-proc
		     (kill-process orphan-proc)))))
	(compilation-minor-mode)
	(ruby-minor-mode)))
    (pop-to-buffer (format "*%s*" name))))

(defun ruby-compilation-insertion-filter (proc string)
  "Insert text to buffer stripping ansi color codes"
  (with-current-buffer (process-buffer proc)
    (let ((moving (= (point) (process-mark proc))))
      (save-excursion
	(goto-char (process-mark proc))
	(insert (ansi-color-filter-apply string))
	(set-marker (process-mark proc) (point)))
      (if moving (goto-char (process-mark proc))))))

(defun ruby-compilation-sentinel (proc msg)
  "Notify to changes in process state"
  (message "%s - %s" proc (replace-regexp-in-string "\n" "" msg)))

(defun ruby-compilation-previous-error-group ()
  "Jump to the start of the previous error group in the current
compilation buffer."
  (interactive)
  (compilation-previous-error 1)
  (while (string-match ruby-compilation-error-regexp (thing-at-point 'line))
    (forward-line -1))
  (forward-line 1) (recenter))

(defun ruby-compilation-next-error-group ()
  "Jump to the start of the previous error group in the current
compilation buffer."
  (interactive)
  (while (string-match ruby-compilation-error-regexp (thing-at-point 'line))
    (forward-line 1))
  (compilation-next-error 1) (recenter))

(provide 'ruby-compilation)
;;; ruby-compilation.el ends here