;;; rinari.el --- Rinari Is Not A Rails IDE

;; Copyright (C) 2008 Phil Hagelberg, Eric Schulte

;; Authors: Phil Hagelberg, Eric Schulte
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
(require 'rinari-movement)

(defcustom rinari-browse-url-func
  'browse-url
  "`browse-url' function used by `rinari-browse-view'.")

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

(defun rinari-parse-yaml ()
  (let ((start (point))
	(end (save-excursion (re-search-forward "^$" nil t) (point)))
	alist)
    (while (and (< (point) end)
		(re-search-forward "^ *\\(.*\\): \\(.*\\)$" nil t))
      (setf alist (cons (cons (match-string 1) (match-string 2)) alist)))
    alist))

(defun rinari-root (&optional dir)
  (or dir (setq dir default-directory))
  (if (file-exists-p (concat dir "config/environment.rb"))
      dir
    (unless (equal dir "/")
      (rinari-root (expand-file-name (concat dir "../"))))))

;;--------------------------------------------------------------------------------
;; user functions

(defun rinari-rake (&optional task edit)
  "Tab completion selection of a rake task to execute with the
output dumped to a compilation buffer allowing jumping between
errors and source code.  With optional prefix argument allows
editing of the rake command."
  (interactive "P")
  (ruby-rake-w/compilation task edit))

(defun rinari-test (&optional edit-command)
  "Test the current ruby function.  If current function is not a
test, then try to jump to the related test using `toggle-buffer'.
Dump output to a compilation buffer allowing jumping between
errors and source code."
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

(defun rinari-console (&optional arg)
  "Run script/console in a compilation buffer, with command
history and links between errors and source code.  Use a prefix
argument to edit command line options."
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

(defun rinari-sql ()
  "Browse the application's database.  Looks up login information
from your conf/database.sql file."
  (interactive)
  (flet ((sql-name (env) (format "*%s-sql*" env)))
    (let* ((environment (or (getenv "RAILS_ENV") "development"))
	   (sql-buffer (get-buffer (sql-name environment))))
      (if sql-buffer
	  (pop-to-buffer sql-buffer)
	(let* ((database-alist (save-excursion
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
	  (eval (list (intern (concat "sql-" adapter))))
	  (rename-buffer (sql-name environment)) (rinari-launch))))))

(defun rinari-web-server ()
  "Run script/server.  Dump output to a compilation buffer
allowing jumping between errors and source code."
  (interactive)
  (let ((default-directory (rinari-root)))
    (ruby-run-w/compilation "script/server")))

(defun rinari-browse-url ()
  "Browse the url of the current view, controller, test, or model
with `rinari-browse-url-func' which defaults to `browse-url'."
  (interactive)
  (unless (equal :view (rinari-whats-my-type))
    (rinari-find-view))
  (let* ((path (buffer-file-name))
	 (route (and (string-match "app/views/\\(.+\\)\.r[ebhtml]+" path)
		     (match-string 1 path)))
	 (port (or () ;; guess port (or not)
		   "3000"))
	 (server (or () ;; guess server (or not)
		     "localhost"))
	 (base (concat server ":" port "/" route))
	 (url (read-from-minibuffer "url: " (concat base "/"))))
    (eval (list rinari-browse-url-func url))))

(defun rinari-insert-erb-skeleton (no-equals)
  "Insert an erb skeleton at point, with optional prefix argument
don't include an '='."
  (interactive "P")
  (insert "<%") (unless no-equals (insert "=")) (insert "  %>")
  (backward-char 3))

(defun rinari-rgrep (&optional arg)
  "Search through the rails project for a string or `regexp'.
With optional prefix argument just run `rgrep'."
  (interactive "P")
  (grep-compute-defaults)
  (if arg (call-interactively 'rgrep)
    (funcall 'rgrep (read-from-minibuffer "search for: ")
	     "*.rb *.rhtml *.yml *.erb" (rinari-root))))

;;--------------------------------------------------------------------
;; minor mode and keymaps

(defvar rinari-minor-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Key map for Rinari minor mode.")

(defvar rinari-minor-mode-keybindings
  '(("o"  . 'toggle-buffer)              ("s" . 'rinari-sql)
    ("e"  . 'rinari-insert-erb-skeleton) ("t" . 'rinari-test)
    ("r"  . 'rinari-rake)                ("c" . 'rinari-console)
    ("w"  . 'rinari-web-server)          ("g" . 'rinari-rgrep)
    ("b" . 'rinari-browse-url)
    ("fc" . 'rinari-find-controller)     ("ft" . 'rinari-find-test)
    ("fv" . 'rinari-find-view)           ("fm" . 'rinari-find-model)
    ("fi" . 'rinari-find-migration)      ("fe" . 'rinari-find-environment)
    ("fj" . 'rinari-find-javascript)     ("fs" . 'rinari-find-stylesheet))
  "alist mapping of keys to functions in `rinari-minor-mode'")

(mapcar (lambda (el)
	  (eval `(define-key rinari-minor-mode-map 
		   ,(format "\C-c;%s" (car el)) ,(cdr el)))
	  (eval `(define-key rinari-minor-mode-map 
		   ,(format "\C-c'%s" (car el)) ,(cdr el))))
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
  (setq toggle-method-format "def %s")
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

;;;###autoload
(define-minor-mode rinari-minor-mode
  "Enable Rinari minor mode providing Emacs support for working
with the Ruby on Rails framework."
  nil
  " Rinari"
  rinari-minor-mode-map)

(provide 'rinari)
;;; rinari.el ends here