;;; rinari-camps.el --- rinari minor mode for camping

(defun rinari-camps-root () default-directory)

(defun rinari-camps-name ()
  (let ((root (rinari-camps-root)))
    (directory-file-name (file-relative-name root (expand-file-name ".." root)))))

(defun rinari-camps-web-server (&optional edit-cmd-args)
  "Run script/server.  Dump output to a compilation buffer
allowing jumping between errors and source code.  With optional
prefix argument allows editing of the server command arguments."
  (interactive "P")
  (let* ((default-directory (rinari-camps-root))
	 (command (format "/usr/bin/camping %s.rb" (rinari-camps-name)))
	 (command (if edit-cmd-args
		      (read-string "" (concat command " "))
		    command)))
    (ruby-run-w/compilation command)))

(defun rinari-camps-console (&optional edit-cmd-args)
  "Run script/console in a compilation buffer, with command
history and links between errors and source code.  With optional
prefix argument allows editing of the console command arguments."
  (interactive "P")
  (let* ((req (expand-file-name (concat (rinari-camps-name) ".rb") (rinari-camps-root))))
    (run-ruby (format "irb -r %s --simple-prompt" req))
    (save-excursion
      (pop-to-buffer "*ruby*")
      (set (make-local-variable 'inferior-ruby-first-prompt-pattern) "^>> ")
      (set (make-local-variable 'inferior-ruby-prompt-pattern) "^>> ")
      (rinari-camps-minor-mode))))

;; minor mode definition
(defvar rinari-camps-minor-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Key map for Rinari Camps minor mode.")

(defun rinari-camps-bind-key-to-func (key func)
  (eval `(define-key rinari-camps-minor-mode-map 
	   ,(format "\C-c;%s" key) ,func))
  (eval `(define-key rinari-camps-minor-mode-map 
	   ,(format "\C-c'%s" key) ,func)))

(defvar rinari-camps-minor-mode-keybindings
  '(("c" . 'rinari-camps-console)
    ("w" . 'rinari-camps-web-server))
  "alist mapping of keys to functions in `rinari-camps-minor-mode'")

(mapcar (lambda (el) (rinari-camps-bind-key-to-func (car el) (cdr el)))
	rinari-camps-minor-mode-keybindings)

;;;###autoload
(define-minor-mode rinari-camps-minor-mode
  "Enable Rinari camps minor mode providing Emacs support for
working with the Camping framework."
  nil
  " Rinari-Camps"
  rinari-camps-minor-mode-map)

(defun rinari-camps ()
  "Launch a minimal rinari-minor-mode for Camping."
  (interactive)
  (rinari-camps-minor-mode)
  (message "you're camping with rinari"))

(provide 'rinari-camps)
;;; rinari-camps.el ends here