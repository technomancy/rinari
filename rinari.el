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

;;; Todo:

;; Name functions consistently
;; Make rinari a minor mode that doesn't activate for regular ruby-mode.
;; make `rinari-find-action' work with rails2-style view filenames
;; make `rinari-find-action' which will follow forms (maybe w/prefix?)
;; add key-bindings for rinari minor mode

;;; Code:

(require 'cl)
(require 'which-func)
(require 'ruby-mode)
(require 'inf-ruby)
(require 'ruby-compilation)
(require 'toggle)

(require 'find-file-in-project)
(require 'pcmpl-rake)

;;;###autoload
(defun rinari-rake (&optional arg)
  (interactive "P")
  (let* ((task (completing-read "Rake: "
				(pcmpl-rake-tasks)))
	 (rake-args (if arg
			(read-from-minibuffer "rake "
					      task)
		      task)))
    (message "%s" (shell-command-to-string (concat "rake " rake-args)))))

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
      (set (make-local-variable 'inferior-ruby-prompt-pattern) "^>> "))))

(defun rinari-server ()
  "Run script/server."
  (interactive)
  (ruby-run-w/compilation (concat (rinari-root) "/script/server")))

(defun rinari-find-view ()
  "View toggling for rails"
  (interactive)
  (let* ((funname (which-function))
	 (function (and (string-match "#\\(.*\\)" funname) (match-string 1 funname)))
	 (controller (rinari-make-dirname (rinari-name-components funname)))
	 (path (rinari-path-to-view controller function))
 	 (appdir (concat (rinari-root) "/app/")))
    (find-file (concat appdir "views/" path ".rhtml"))))

(defun rinari-path-to-view (controller function)
  "Takes a CONTROLLER and FUNCTION and returns the path to the
view at which CONTROLLER#FUNCTION points."
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

(defvar rinari-ruby-hash-regexp
  "\\(:[^[:space:]]*?\\)[[:space:]]*\\(=>[[:space:]]*[\"\']?\\([^[:space:]]*?\\)[\"\']?[[:space:]]*\\)?[,){}\n]"
  ;; "\\(:[^[:space:]]*?\\)[[:space:]]*\\(=>[[:space:]]*\\([^[:space:]]*\\)[[:space:]]*\\)?[,){}$]"
  "Regexp to match subsequent key => value pairs of a ruby hash.")

(defun rinari-ruby-hash-to-alist ()
  "Returns an alist of the key => value pairs on consecutive
lines starting at point."
  (let ((end (save-excursion
	       (re-search-forward "[^,{(]$" nil t)
	       (+ 1 (point))))
	alist)
    (save-excursion
      (while (and (< (point) end)
		  (re-search-forward rinari-ruby-hash-regexp end t))
	(setf alist
	      (cons (cons (match-string 1)
			  (if (> (length (match-string 3)) 0)
			      (match-string 3)
			    "true"))
		    alist))))
    alist))

(defun rinari-alist-from-form ()
  "If currently inside of a form in a view return an alist of the
  hash key => values, else return nil."
  (interactive)
  (save-excursion
    (if (and (re-search-backward "form_\(?:\(?:remote_\)?tag\)" nil t)
	     (ruby-forward-sexp))
	(rinari-ruby-hash-to-alist))))

;;; TODO: make this work with rails2-style view filenames
(defun rinari-find-action ()
  (interactive)
  (let* ((form-alist (rinari-alist-from-form))
	 (action (or (cdr (assoc ":action" form-alist))
		     (file-name-sans-extension
		      (file-name-nondirectory buffer-file-name))))
	 (controller (or (cdr (assoc ":controller" form-alist))
			 (file-name-nondirectory 
			  (expand-file-name ".")))))
    (find-file (concat (rinari-root)
		       "app/controllers/"
		       controller
		       "_controller.rb"))
    (goto-char (point-min))
    (search-forward-regexp (concat "def *" action))
    (recenter)))

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

(defun rinari-insert-erb-skeleton (no-equals)
  (interactive "P")
  (insert "<%")
  (unless no-equals (insert "="))
  (insert "  %>")
  (backward-char 3))

(defcustom rinari-browse-url-func
  'browse-url
  "`browse-url' function used by `rinari-browse-view'.")

(defun rinari-browse-view (arg)
  "Browse the url of the current view with `rinari-browse-url-func'
which default to `browse-url'.  With a prefix argument allows
editing of the url."
  (interactive "P")
  (let* ((path (buffer-file-name))
	 (view (if (string-match "app/views/\\(.+\\)\.r[ebhtml]+" path)
		   (match-string 1 path)))
	 (port (or () ;; guess port
		   "3000"))
	 (server (or () ;; guess server
		     "localhost"))
	 (base (concat server ":" port "/" view))
	 (url (if arg
		  (read-from-minibuffer "url: "
					(concat base "/"))
		base)))
    (eval (list rinari-browse-url-func url))))

(defadvice find-file-in-project (around find-file-in-rinari-project activate)
  "Wrap `find-file-in-project' to use `rinari-root' as the base of
  the project."
  (let ((ffip-project-root (rinari-root)))
    ad-do-it))

;; ;; TODO: need to figure out how to defadvice with a prefix a
;; ;; function which doesn't normally have a prefix
;; (defadvice toggle-buffer (around toggle-buffer-and-run-test first activate)
;;   "Wrap `toggle-buffer' so that if it is called with a prefix
;;   argument, and the toggle ends on a testing method, then we run
;;   the related test, dumping the results into a compilation
;;   buffer"
;;   (interactive "P")
;;   ad-do-it
;;   (if (ad-get-arg 0)
;;       (let* ((line (thing-at-point 'line))
;; 	     (test (and (string-match "def \\(.+\\)" line)
;; 			(match-string 1 line))))
;; 	(if (string-match "test" test)
;; 	    (ruby-run-w/compilation (concat (buffer-file-name) " -n " test))))))

;;--------------------------------------------------------------------
;;
;; minor mode and keymaps
;; 
(defvar rinari-minor-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Key map for Rinari minor mode.")
(define-key rinari-minor-mode-map "\C-c'r" 'rinari-rake)
(define-key rinari-minor-mode-map "\C-c's" 'rinari-script)
(define-key rinari-minor-mode-map "\C-c'c" 'rinari-console)
(define-key rinari-minor-mode-map "\C-c'w" 'rinari-server)
(define-key rinari-minor-mode-map "\C-c'v" 'rinari-find-view)
(define-key rinari-minor-mode-map "\C-c'a" 'rinari-find-action)
(define-key rinari-minor-mode-map "\C-c'b" 'rinari-browse-view)
(define-key rinari-minor-mode-map "\C-c'e" 'rinari-insert-erb-skeleton)
(define-key rinari-minor-mode-map "\C-c't" 'toggle-buffer)

(defun rinari-launch ()
  "Run `rinari-minor-mode' if inside of a rails projcect"
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
  (if (rinari-root) (rinari-minor-mode t)))

(add-hook 'ruby-mode-hook
	  (lambda () (rinari-launch)))

(add-hook 'mumamo-after-change-major-mode-hook
	  (lambda () (rinari-launch)))

(define-minor-mode rinari-minor-mode
  "Enable Rinari minor mode providing Emacs support for working
with the Ruby on Rails framework."
  nil
  " Rinari"
  rinari-minor-mode-map)

(provide 'rinari)
;;; rinari.el ends here