;;; rinari.el --- Rinari Is Not A Rails IDE

;; Copyright (C) 2008 Phil Hagelberg

;; Author: Phil Hagelberg
;; URL: http://rinari.rubyforge.org
;; Version: 2.0
;; Created: 2006-11-10
;; Keywords: ruby, rails
;; EmacsWiki: Rinari

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Rinari Is Not A Ruby IDE.

;; Well, ok it kind of is. Rinari is a set of Emacs Lisp modes that is
;; aimed towards making Emacs into a top-notch Ruby and Rails
;; development environment.

;; Copy the directory containing this file into your Emacs lisp
;; directory, assumed here to be ~/.emacs.d. Add these lines of code
;; to your .emacs file:

;; (add-to-list 'load-path "~/.emacs.d/rinari")
;; (require 'rinari)

;; See TODO file in this directory.

;;; Code:
(require 'ruby-mode)
(require 'inf-ruby)
(require 'ruby-compilation)
(require 'cl)
(require 'toggle)
(require 'find-file-in-project)

(defcustom rinari-browse-url-func
  'browse-url
  "`browse-url' function used by `rinari-browse-view'.")

(defvar rinari-ruby-hash-regexp
  "\\(:[^[:space:]]*?\\)[[:space:]]*\\(=>[[:space:]]*[\"\']?\\([^[:space:]]*?\\)[\"\']?[[:space:]]*\\)?[,){}\n]"
  "Regexp to match subsequent key => value pairs of a ruby hash.")

(defun rinari-ruby-hash-to-alist ()
  "Returns an alist of the key => value pairs on consecutive
lines starting at point."
  (let (alist
	(end (save-excursion
	       (re-search-forward "[^,{(]$" nil t)
	       (+ 1 (point)))))
    (save-excursion
      (while (and (< (point) end)
		  (re-search-forward rinari-ruby-hash-regexp end t))
	(setf alist
	      (cons (cons (match-string 1)
			  (if (> (length (match-string 3)) 0)
			      (match-string 3)
			    "true"))
		    alist)))) alist))

(defun rinari-alist-from-view ()
  "Return an alist of the options directing to an action from the
current view.  Taken from forms or links."
  (interactive)
  (save-excursion
    (let ((form_re (regexp-opt '("form_remote_tag" "form_tag")))
	  (link_re (regexp-opt '("link_to" "link_to_remote"))))
      (and (or (re-search-backward form_re nil t) (re-search-forward link_re nil t)
	       (re-search-backward link_re nil t) (re-search-backward form_re nil t))
	 (rinari-ruby-hash-to-alist)))))

(defun rinari-path-to-view (controller function)
  "Takes a CONTROLLER and FUNCTION and returns the path to the
view at which CONTROLLER#FUNCTION points."
  (unless (and controller function)
    (error "can't find view without controller and function"))
  (let (path)
    (save-excursion
      (find-file (concat controller "_controller.rb"))
      (goto-char (point-max))
      ;; if we can find function in this controller
      (if (re-search-backward (format "def[[:space:]]+%s" function) nil t)
	  (let ((start (point)) render renders view)
	    (ruby-forward-sexp)
	    ;; collect all the render/redirects
	    (while (re-search-backward "re\\(?:direct_to\\|nder\\)[^_]" start t)
	      (setf renders (cons (cons (replace-regexp-in-string
					 "[[:space:]\n\r]" ""
					 (thing-at-point 'line)) (point)) renders)))
	    (if renders
		;; if method contains render/redirects select one and follow it
		(let ((render (if (equal 1 (length renders))
				  (caar renders)
				(completing-read "follow which render: "
						 (mapcar 'car renders)))))
		  (goto-char (cdr (assoc render renders)))
		  ;; read the hashed arguments to the redirect
		  (let* ((redirect (rinari-ruby-hash-to-alist))
			 (new-function (or (cdr (assoc ":action" redirect))
					   (if (assoc ":partial" redirect)
					       (concat "_" (cdr (assoc ":partial" redirect))))
					   function))
			 (new-controller (or (cdr (assoc ":controller" redirect))
					     controller)))
		    ;; if we are pointed at a new action, check it for
		    ;; redirects otherwise return a path
		    (if (and (equalp new-function function)
			     (equalp new-controller controller))
			(setf path (concat controller "/" function))
		      (setf path (rinari-path-to-view new-controller new-function)))))
	      (if (search-backward "render_partial" start t)
		  (setf path (concat controller "/" "_" function)))))))
    (or path (concat controller "/" function))))

(defun rinari-name-components (name)
  "Helper for view toggling"
  (let ((case-fold-search nil))
    (labels ((rnc (in)
		  (let ((ind (string-match "\\([A-Z][a-z0-9]+\\)[A-Z]" name in)))
		    (if (eq ind nil)
			nil
		      (cons (downcase (match-string 1 name)) (rnc (match-end 1)))))))
      (rnc 0))))

(defun rinari-make-dirname (comps)
  "Helper for view toggling"
  (reduce #'(lambda (str next) (concat str (concat "_" next))) comps))

(defadvice find-file-in-project (around find-file-in-rinari-project activate)
  "Wrap `find-file-in-project' to use `rinari-root' as the base of
  the project."
  (let ((ffip-project-root (rinari-root)))
    ad-do-it))

(defadvice ruby-run-w/compilation (around rinari-run-w/compilation activate)
  "Set default directory to the root of the rails application
  before running ruby processes."
  (let ((default-directory (or (rinari-root) default-directory)))
    ad-do-it
    (rinari-launch)))

(defadvice ruby-rake-w/compilation (around rinari-rake-w/compilation activate)
  "Set default directory to the root of the rails application
  before running rake processes."
  (let ((default-directory (or (rinari-root) default-directory)))
    ad-do-it
    (rinari-launch)))

;;;###autoload
(defun rinari-rake (&optional task edit)
  (interactive "P")
  (ruby-rake-w/compilation task edit))

(defun rinari-root (&optional dir)
  (or dir (setq dir default-directory))
  (if (file-exists-p (concat dir "config/environment.rb"))
      dir
    (unless (equal dir "/")
      (rinari-root (expand-file-name (concat dir "../"))))))

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
      (set (make-local-variable 'inferior-ruby-prompt-pattern) "^>> ")
      (rinari-launch))))

(defun rinari-server ()
  "Run script/server."
  (interactive)
  (ruby-run-w/compilation (concat (rinari-root) "/script/server")))

(defun rinari-find-view ()
  "View toggling for rails"
  (interactive)
  (let* ((fn (ruby-add-log-current-method))
	 (function (and fn (string-match "#\\(.*\\)" fn) (match-string 1 fn)))
	 (controller (and fn (rinari-make-dirname (rinari-name-components fn))))
	 (path (rinari-path-to-view controller function))
 	 (appdir (concat (rinari-root) "/app/")))
    (find-file (concat appdir "views/" path ".rhtml"))))

(defun rinari-find-action ()
  (interactive)
  (let* ((view-alist (rinari-alist-from-view))
	 (action (or (cdr (assoc ":action" view-alist))
		     (file-name-sans-extension
		      (file-name-nondirectory buffer-file-name))))
	 (controller (or (cdr (assoc ":controller" view-alist))
			 (file-name-nondirectory
			  (expand-file-name ".")))))
    (find-file (concat (rinari-root)
		       "app/controllers/"
		       controller
		       "_controller.rb"))
    (goto-char (point-min))
    (search-forward-regexp (concat "def *" action))
    (recenter)))

(defun rinari-test-function (&optional edit-command)
  "Test the current ruby function.  If current function is not a
test, then try to jump to the related test using `toggle-buffer'."
  (interactive "P")
  (or (string-match "test" (or (ruby-add-log-current-method)
			       (file-name-nondirectory (buffer-file-name))))
      (toggle-buffer))
  (let* ((funname (ruby-add-log-current-method))
	 (fn (and funname
		  (string-match "#\\(.*\\)" funname)
		  (match-string 1 funname)))
	 (path (buffer-file-name))
	 (default-command (if fn
			      (concat path " --name /" fn "/")
			    path))
	 (command (if edit-command
		      (read-string "Run w/Compilation: " default-command)
		    default-command)))
    (if path (ruby-run-w/compilation command)
      (message "no test available"))))

;; TODO: maybe replace this with a snippets package; needs research
(defun rinari-insert-erb-skeleton (no-equals)
  (interactive "P")
  (insert "<%")
  (unless no-equals (insert "="))
  (insert "  %>")
  (backward-char 3))

(defun rinari-browse-view (arg)
  "Browse the url of the current view with `rinari-browse-url-func'
which default to `browse-url'.  With a prefix argument allows
editing of the url."
  (interactive "P")
  (let* ((path (buffer-file-name))
	 (view (if (string-match "app/views/\\(.+\\)\.r[ebhtml]+" path)
		   (match-string 1 path)))
	 (port (or () ;; guess port (or not)
		   "3000"))
	 (server (or () ;; guess server (or not)
		     "localhost"))
	 (base (concat server ":" port "/" view))
	 (url (if arg
		  (read-from-minibuffer "url: "
					(concat base "/"))
		base)))
    (eval (list rinari-browse-url-func url))))

(defun rinari-parse-yaml ()
  (let ((start (point))
	(end (save-excursion (re-search-forward "^$" nil t) (point)))
	alist)
    (while (and (< (point) end)
		(re-search-forward "^ *\\(.*\\): \\(.*\\)$" nil t))
      (setf alist (cons (cons (match-string 1) (match-string 2)) alist)))
    alist))

(defun rinari-sql ()
  "Browse the application's database."
  (interactive)
  (let* ((environment (or (getenv "RAILS_ENV") "development"))
	 (database-alist (save-excursion
			   (with-temp-buffer
			     (insert-file (concat (rinari-root)
					      "/config/database.yml"))
			     (goto-char (point-min))
			     (re-search-forward (concat "^" environment ":"))
			     (rinari-parse-yaml))))
	 (adapter (or (cdr (assoc "adapter" database-alist)) "sqlite"))
	 (sql-user (or (cdr (assoc "username" database-alist)) "root"))
	 (sql-password (or (cdr (assoc "password" database-alist)) ""))
	 (sql-database (or (cdr (assoc "database" database-alist))
			   (concat (file-name-nondirectory (rinari-root))
				   "_" environment)))
	 (server (or (cdr (assoc "host" database-alist)) "localhost"))
	 (port (cdr (assoc "port" database-alist)))
	 (sql-server (if port (concat server ":" port) server)))
    (if (string-match "sqlite" adapter) (setf adapter "sqlite"))
    (eval (list (intern (concat "sql-" adapter)))) (rinari-launch)))

(defun rinari-run-what (&optional arg)
  "Allows the user to run a function selected from amongst all of
the rinari functions displaying their names and keybindings."
  (interactive "P")
  (let* ((rinari-commands (mapcar (lambda (el)
				    (format "%s (\\C-c'%s)"
					    (caddr el) (car el)))
				  rinari-minor-mode-keybindings))
	 (rinari-command (car (split-string
			       (completing-read "run rinari function: "
						rinari-commands)))))
    (call-interactively (intern rinari-command))))

;;--------------------------------------------------------------------
;;
;; minor mode and keymaps
;;
(defvar rinari-minor-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Key map for Rinari minor mode.")

(defvar rinari-minor-mode-keybindings
  '(("\t" . 'rinari-run-what) ("o" . 'toggle-buffer) ("s" . 'rinari-sql)
    ("e" . 'rinari-insert-erb-skeleton) ("t" . 'rinari-test-function)
    ("r" . 'rinari-rake) ("c" . 'rinari-console) ("b" . 'rinari-browse-view)
    ("v" . 'rinari-find-view) ("a" . 'rinari-find-action) ("w" . 'rinari-server))
  "alist mapping of keys to functions in `rinari-minor-mode'")

(mapcar (lambda (el)
	  (eval `(define-key rinari-minor-mode-map ,(format "\C-c'%s" (car el)) ,(cdr el))))
	rinari-minor-mode-keybindings)

(defun rinari-launch ()
  "Run `rinari-minor-mode' if inside of a rails projcect,
otherwise turn `rinari-minor-mode' off if it is on."
  (interactive)
  ;; customize toggle.el for rinari
  (add-to-list
   'toggle-mapping-styles
   '(rinari  . (("app/controllers/\\1.rb#\\2" . "test/functional/\\1_test.rb#test_\\2")
		("app/controllers/\\1.rb"     . "test/functional/\\1_test.rb")
		("app/models/\\1.rb#\\2"      . "test/unit/\\1_test.rb#test_\\2")
		("app/models/\\1.rb"          . "test/unit/\\1_test.rb")
		("lib/\\1.rb#\\2"             . "test/unit/test_\\1.rb#test_\\2")
		("lib/\\1.rb"                 . "test/unit/test_\\1.rb"))))
  (setq toggle-mapping-style 'rinari)
  (setq toggle-mappings (toggle-style toggle-mapping-style))
  (setq toggle-which-function-command 'ruby-add-log-current-method)
  (if (rinari-root)
      (unless rinari-minor-mode (rinari-minor-mode t))
      (if rinari-minor-mode (rinari-minor-mode))))

(add-hook 'ruby-mode-hook
	  (lambda () (rinari-launch)))

(add-hook 'yaml-mode-hook
	  (lambda () (rinari-launch)))

(add-hook 'mumamo-after-change-major-mode-hook
	  (lambda () (rinari-launch)))

(add-hook 'dired-mode-hook
	  (lambda () (rinari-launch)))

(defadvice cd (after rinari-on-cd activate)
  "Active/Deactive rinari-minor-node when changing into and out
  of raills project directories."
  (rinari-launch))

(define-minor-mode rinari-minor-mode
  "Enable Rinari minor mode providing Emacs support for working
with the Ruby on Rails framework."
  nil
  " Rinari"
  rinari-minor-mode-map)

(provide 'rinari)
;;; rinari.el ends here