;;; inf-ruby.el --- Run a ruby process in a buffer

;; Copyright (C) 1999-2008 Yukihiro Matsumoto, Nobuyoshi Nakada

;; Authors: Yukihiro Matsumoto, Nobuyoshi Nakada
;; URL: http://www.emacswiki.org/cgi-bin/wiki/RubyMode
;; Created: 8 April 1998
;; Keywords: languages ruby
;; Version: 1.7

;;; Commentary:
;;
;; inf-ruby.el provides a REPL buffer connected to an IRB subprocess.
;;
;; If you're installing manually, you'll need to:
;; * drop the file somewhere on your load path (perhaps ~/.emacs.d)
;; * Add the following lines to your .emacs file:
;;    (autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process" t)
;;    (autoload 'inf-ruby-keys "inf-ruby" "" t)
;;    (eval-after-load 'ruby-mode
;;      '(add-hook 'ruby-mode-hook 'inf-ruby-keys))

;;; TODO:
;;
;; Can you autoload an eval-after-load?
;; What's the meaning of an asterisk at the start of a docstring?
;; inferior-ruby-error-regexp-alist doesn't match this example
;;   SyntaxError: /home/eschulte/united/org/work/arf/arf/lib/cluster.rb:35: syntax error, unexpected '~', expecting kEND
;;               similarity = comparison_cache[m][n] ||= clusters[m] ~ clusters[n]
;;               

(require 'comint)
(require 'compile)
(require 'ruby-mode)

(defvar inf-ruby-executable "irb --inf-ruby-mode"
  "*Program invoked by the run-ruby command")

(defvar inf-ruby-first-prompt-pattern "^irb(.*)[0-9:]+0> *"
  "First prompt regex pattern of ruby interpreter.")

(defvar inf-ruby-prompt-pattern "^\\(irb(.*)[0-9:]+[>*\"'] *\\)+"
  "Prompt regex pattern of ruby interpreter.")

(defvar inf-ruby-mode-hook nil
  "*Hook for customising inf-ruby mode.")

(defvar inf-ruby-mode-map
  (let ((map (copy-keymap comint-mode-map)))
    (define-key map (kbd "C-c C-l") 'inf-ruby-load-file)
    map)
  "*Mode map for inf-ruby-mode")

(defvar inf-ruby-implementations
  '(("ruby"     . "irb")
    ("jruby"    . "jruby -S irb")
    ("rubinius" . "rbx")
    ("yarv"     . "irb1.9"))
  "An alist of ruby implementations to irb executable names.")

(defvar ruby-source-modes '(ruby-mode)
  "*Used to determine if a buffer contains Ruby source code.
If it's loaded into a buffer that is in one of these major modes, it's
considered a ruby source file by ruby-load-file.
Used by these commands to determine defaults.")

(defvar ruby-prev-l/c-dir/file nil
  "Caches the last (directory . file) pair.
Caches the last pair used in the last ruby-load-file command.
Used for determining the default in the 
next one.")

(defconst inf-ruby-error-regexp-alist
       '(("SyntaxError: compile error\n^\\([^\(].*\\):\\([1-9][0-9]*\\):" 1 2)
	 ("^\tfrom \\([^\(].*\\):\\([1-9][0-9]*\\)\\(:in `.*'\\)?$" 1 2)))

       (define-key inf-ruby-mode-map "\M-\C-x" ;gnu convention
         'ruby-send-definition)
       (define-key inf-ruby-mode-map "\C-x\C-e" 'ruby-send-last-sexp)
       (define-key inf-ruby-mode-map "\C-c\C-l" 'ruby-load-file)

;;;###autoload
(defun inf-ruby-keys ()
  "Set local key defs for inf-ruby in ruby-mode"
  (define-key ruby-mode-map "\M-\C-x" 'ruby-send-definition)
  (define-key ruby-mode-map "\C-x\C-e" 'ruby-send-last-sexp)
  (define-key ruby-mode-map "\C-c\C-b" 'ruby-send-block)
  (define-key ruby-mode-map "\C-c\M-b" 'ruby-send-block-and-go)
  (define-key ruby-mode-map "\C-c\C-x" 'ruby-send-definition)
  (define-key ruby-mode-map "\C-c\M-x" 'ruby-send-definition-and-go)
  (define-key ruby-mode-map "\C-c\C-r" 'ruby-send-region)
  (define-key ruby-mode-map "\C-c\M-r" 'ruby-send-region-and-go)
  (define-key ruby-mode-map "\C-c\C-z" 'switch-to-ruby)
  (define-key ruby-mode-map "\C-c\C-l" 'ruby-load-file)
  (define-key ruby-mode-map "\C-c\C-s" 'run-ruby))

(defvar inf-ruby-buffer nil "current ruby (actually irb) process buffer.")

(defun inf-ruby-mode ()
  "Major mode for interacting with an inferior ruby (irb) process.

The following commands are available:
\\{inf-ruby-mode-map}

A ruby process can be fired up with M-x run-ruby.

Customisation: Entry to this mode runs the hooks on comint-mode-hook and
inf-ruby-mode-hook (in that order).

You can send text to the inferior ruby process from other buffers containing
Ruby source.
    switch-to-ruby switches the current buffer to the ruby process buffer.
    ruby-send-definition sends the current definition to the ruby process.
    ruby-send-region sends the current region to the ruby process.

    ruby-send-definition-and-go, ruby-send-region-and-go,
        switch to the ruby process buffer after sending their text.
For information on running multiple processes in multiple buffers, see
documentation for variable inf-ruby-buffer.

Commands:
Return after the end of the process' output sends the text from the 
    end of process to point.
Return before the end of the process' output copies the sexp ending at point
    to the end of the process' output, and sends it.
Delete converts tabs to spaces as it moves back.
Tab indents for ruby; with argument, shifts rest
    of expression rigidly with the current line.
C-M-q does Tab on each line starting within following expression.
Paragraphs are separated only by blank lines.  # start comments.
If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it."
  (interactive)
  (comint-mode)
  ;; Customise in inf-ruby-mode-hook
  ;(setq comint-prompt-regexp "^[^>\n]*>+ *")
  (setq comint-prompt-regexp inf-ruby-prompt-pattern)
  ;;(scheme-mode-variables)
  (ruby-mode-variables)
  (setq major-mode 'inf-ruby-mode)
  (setq mode-name "Inf-Ruby")
  (setq mode-line-process '(":%s"))
  (use-local-map inf-ruby-mode-map)
  (setq comint-input-filter (function ruby-input-filter))
  (setq comint-get-old-input (function ruby-get-old-input))
  (make-local-variable 'compilation-error-regexp-alist)
  (setq compilation-error-regexp-alist inf-ruby-error-regexp-alist)
  (compilation-shell-minor-mode t)
  (run-hooks 'inf-ruby-mode-hook))

(defvar inf-ruby-filter-regexp "\\`\\s *\\S ?\\S ?\\s *\\'"
  "*Input matching this regexp are not saved on the history list.
Defaults to a regexp ignoring all inputs of 0, 1, or 2 letters.")

(defun ruby-input-filter (str)
  "Don't save anything matching inf-ruby-filter-regexp"
  (not (string-match inf-ruby-filter-regexp str)))

;; adapted from replace-in-string in XEmacs (subr.el)
(defun remove-in-string (str regexp)
  "Remove all matches in STR for REGEXP and returns the new string."
  (let ((rtn-str "") (start 0) match prev-start)
    (while (setq match (string-match regexp str start))
      (setq prev-start start
	    start (match-end 0)
	    rtn-str (concat rtn-str (substring str prev-start match))))
    (concat rtn-str (substring str start))))

(defun ruby-get-old-input ()
  "Snarf the sexp ending at point"
  (save-excursion
    (let ((end (point)))
      (re-search-backward inf-ruby-first-prompt-pattern)
      (remove-in-string (buffer-substring (point) end)
			inf-ruby-prompt-pattern)
      )))

(defun ruby-args-to-list (string)
  (let ((where (string-match "[ \t]" string)))
    (cond ((null where) (list string))
	  ((not (= where 0))
	   (cons (substring string 0 where)
		 (ruby-args-to-list (substring string (+ 1 where)
						 (length string)))))
	  (t (let ((pos (string-match "[^ \t]" string)))
	       (if (null pos)
		   nil
		 (ruby-args-to-list (substring string pos
						 (length string)))))))))
;;;###autoload
(defun run-ruby (cmd)
  "Run an inferior Ruby process, input and output via buffer *ruby*.
If there is a process already running in `*ruby*', switch to that buffer.
With argument, allows you to edit the command line (default is value
of `inf-ruby-executable').  Runs the hooks `inf-ruby-mode-hook'
\(after the `comint-mode-hook' is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)"

  (interactive (list (if current-prefix-arg
			 (read-string "Run Ruby: " inf-ruby-executable)
			 inf-ruby-executable)))
  (if (not (comint-check-proc "*ruby*"))
      (let ((cmdlist (ruby-args-to-list cmd)))
	(set-buffer (apply 'make-comint "ruby" (car cmdlist)
			   nil (cdr cmdlist)))
	(inf-ruby-mode)))
  (setq inf-ruby-executable cmd)
  (setq inf-ruby-buffer "*ruby*")
  (pop-to-buffer "*ruby*"))

(defconst ruby-send-terminator "--inf-ruby-%x-%d-%d-%d--"
  "Template for irb here document terminator.
Must not contain ruby meta characters.")

(defconst ruby-eval-separator "")

(defun ruby-send-region (start end)
  "Send the current region to the inferior Ruby process."
  (interactive "r")
  (let (term (file (buffer-file-name)) line)
    (save-excursion
      (save-restriction
	(widen)
	(goto-char start)
	(setq line (+ start (forward-line (- start)) 1))
	(goto-char start)
	(while (progn
		 (setq term (apply 'format ruby-send-terminator (random) (current-time)))
		 (re-search-forward (concat "^" (regexp-quote term) "$") end t)))))
    ;; compilation-parse-errors parses from second line.
    (save-excursion
      (let ((m (process-mark (inf-ruby-proc))))
	(set-buffer (marker-buffer m))
	(goto-char m)
	(insert ruby-eval-separator "\n")
	(set-marker m (point))))
    (comint-send-string (inf-ruby-proc) (format "eval <<'%s', nil, %S, %d\n" term file line))
    (comint-send-region (inf-ruby-proc) start end)
    (comint-send-string (inf-ruby-proc) (concat "\n" term "\n"))))

(defun ruby-send-definition ()
  "Send the current definition to the inferior Ruby process."
  (interactive)
  (save-excursion
    (ruby-end-of-defun)
    (let ((end (point)))
      (ruby-beginning-of-defun)
      (ruby-send-region (point) end))))

;(defun ruby-send-last-sexp ()
;  "Send the previous sexp to the inferior Ruby process."
;  (interactive)
;  (ruby-send-region (save-excursion (backward-sexp) (point)) (point)))

(defun ruby-send-block ()
  "Send the current block to the inferior Ruby process."
  (interactive)
  (save-excursion
    (ruby-end-of-block)
    (end-of-line)
    (let ((end (point)))
      (ruby-beginning-of-block)
      (ruby-send-region (point) end))))

(defun switch-to-ruby (eob-p)
  "Switch to the ruby process buffer.
With argument, positions cursor at end of buffer."
  (interactive "P")
  (if (get-buffer inf-ruby-buffer)
      (pop-to-buffer inf-ruby-buffer)
      (error "No current process buffer. See variable inf-ruby-buffer."))
  (cond (eob-p
	 (push-mark)
	 (goto-char (point-max)))))

(defun ruby-send-region-and-go (start end)
  "Send the current region to the inferior Ruby process.
Then switch to the process buffer."
  (interactive "r")
  (ruby-send-region start end)
  (switch-to-ruby t))

(defun ruby-send-definition-and-go ()
  "Send the current definition to the inferior Ruby. 
Then switch to the process buffer."
  (interactive)
  (ruby-send-definition)
  (switch-to-ruby t))

(defun ruby-send-block-and-go ()
  "Send the current block to the inferior Ruby. 
Then switch to the process buffer."
  (interactive)
  (ruby-send-block)
  (switch-to-ruby t))

(defun ruby-load-file (file-name)
  "Load a Ruby file into the inferior Ruby process."
  (interactive (comint-get-source "Load Ruby file: " ruby-prev-l/c-dir/file
				  ruby-source-modes t)) ;; T because LOAD needs an exact name
  (comint-check-source file-name) ; Check to see if buffer needs saved.
  (setq ruby-prev-l/c-dir/file (cons (file-name-directory    file-name)
				       (file-name-nondirectory file-name)))
  (comint-send-string (inf-ruby-proc) (concat "(load \""
					    file-name
					    "\"\)\n")))

(defun inf-ruby-proc ()
  "Returns the current IRB process. See variable inf-ruby-buffer."
  (or (get-buffer-process (if (eq major-mode 'inf-ruby-mode)
                              (current-buffer)
                            inf-ruby-buffer))
      (error "No current process. See variable inf-ruby-buffer")))

(provide 'inf-ruby)
;;; inf-ruby.el ends here
