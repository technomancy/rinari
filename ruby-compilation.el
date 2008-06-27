;;; ruby-compilation.el --- run a ruby process in a compilation buffer

;; Eric Schulte

;;; Code:
(require 'ansi-color)
(require 'pcmpl-rake)

(defvar ruby-compilation-error-regexp
  "^\\([[:space:]]*\\|.*\\[\\|.*at \\)\\[?\\([^[:space:]]*\\):\\([[:digit:]]+\\)[]:)]"
  "regular expression to match errors in ruby process output")

(defvar ruby-compilation-error-regexp-alist
  `((,ruby-compilation-error-regexp 2 3))
  "a version of `compilation-error-regexp-alist' to be used in
  rails logs (should be used with `make-local-variable')")

(defun ruby-run-w/compilation (cmd)
  "Run a ruby process dumping output to a ruby compilation buffer."
  (interactive "FRuby Comand: ")
  (let ((name (file-name-nondirectory (car (split-string cmd))))
	(cmdlist (cons "ruby" (ruby-args-to-list (expand-file-name cmd)))))
    (pop-to-buffer (ruby-do-run-w/compilation name cmdlist))))

(defun ruby-rake-w/compilation (&optional task edit)
  "Run a rake process dumping output to a ruby compilation buffer."
  (interactive "P")
  (let* ((task (or task (completing-read "Rake: " (pcmpl-rake-tasks))))
	 (rake-args (if edit (read-from-minibuffer "rake " task) task)))
    (pop-to-buffer (ruby-do-run-w/compilation
		    "rake" (cons "/usr/local/bin/rake"
				 (ruby-args-to-list rake-args))))))

(defun ruby-do-run-w/compilation (name cmdlist)
  (let ((comp-buffer-name (format "*%s*" name)))
    (unless (comint-check-proc comp-buffer-name)
      (if (get-buffer comp-buffer-name) (kill-buffer comp-buffer-name))
      (let* ((buffer (apply 'make-comint name (car cmdlist) nil (cdr cmdlist)))
	     (proc (get-buffer-process buffer)))
	(save-excursion ;; set buffer local variables
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
	  ;; bind keys (maybe should be implemented as a ruby-compilation minor mode)
	  (local-set-key "p"    'previous-error-no-select)
	  (local-set-key "n"    'next-error-no-select)
	  (local-set-key "\M-p" 'ruby-compilation-previous-error-group)
	  (local-set-key "\M-n" 'ruby-compilation-next-error-group))
	buffer))))

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