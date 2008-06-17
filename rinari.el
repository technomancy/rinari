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
;; make `rails-find-action' work with rails2-style view filenames
;; make `rails-find-action' which will follow forms (maybe w/prefix?)
;; add key-bindings for rinari minor mode

;;; Code:

(require 'cl)
(require 'which-func)
(require 'ruby-mode)
(require 'inf-ruby)
(require 'toggle)

(require 'find-file-in-project)
(require 'pcmpl-rake)
(require 'rails-script)

;;;###autoload
(defun rails-rake (task)
  (interactive (list (completing-read "Rake (default: default): "
				      (pcmpl-rake-tasks))))
  (shell-command-to-string (concat "rake " (if (= 0 (length task)) "default" task))))

(defun rails-root (&optional dir)
  (or dir (setq dir default-directory))
  (if (file-exists-p (concat dir "config/environment.rb"))
      dir
    (unless (equal dir "/")
      (rails-root (expand-file-name (concat dir "../"))))))

(defun rails-find-view ()
  "View toggling for rails"
  (interactive)
  (let* ((funname (which-function))
	 (fn (and (string-match "#\\(.*\\)" funname) (match-string 1 funname)))
	 (cls (rails-make-dirname (rails-name-components funname)))
	 (path (rails-pointing-at-view cls fn))
 	 (appdir (file-name-directory
		  (directory-file-name
		   (file-name-directory (buffer-file-name))))))
    (find-file (concat appdir "views/" path ".rhtml"))))

(defun rails-pointing-at-view (controller function)
  "Takes a CONTROLLER and FUNCTION and returns the path to the
view at which CONTROLLER#FUNCTION points."
  (interactive)
  (let (path)
    (save-excursion
      (find-file (concat controller "_controller.rb"))
      (goto-char (point-max))
      (if (re-search-backward (format "def[[:space:]]+%s" function) nil t)
	  (let ((start (point)) render renders view)
	    (ruby-forward-sexp)
	    (while (re-search-backward "re\\(?:direct_to\\|nder\\)[^_]" start t)
	      (setf renders (cons (cons (replace-regexp-in-string
					 "[[:space:]\n\r]" ""
					 (thing-at-point 'line)) (point)) renders)))
	    (if renders
		(let ((render (if (equal 1 (length renders))
				  (caar renders)
				(completing-read "follow which render: "
						 (mapcar 'car renders)))))
		  (goto-char (cdr (assoc render renders)))
		  (let* ((redirect (rails-ruby-hash-to-alist))
			 (new-function (or (cdr (assoc ":action" redirect))
					   (if (assoc ":partial" redirect)
					       (concat "_" (cdr (assoc ":partial" redirect))))
					   function))
			 (new-controller (or (cdr (assoc ":controller" redirect))
					     controller)))
		    (if (and (equalp new-function function)
			     (equalp new-controller controller))
			(setf path (concat controller "/" function))
		      (setf path (rails-pointing-at-view new-controller new-function)))))
	      (if (search-backward "render_partial" start t)
		  (setf path (concat controller "/" "_" function)))))))
    (or path (concat controller "/" function))))

(defvar rails-ruby-hash-regexp
  "\\(:[^[:space:]]*?\\)[[:space:]]*\\(=>[[:space:]]*[\"\']?\\([^[:space:]]*?\\)[\"\']?[[:space:]]*\\)?[,){}\n]"
  ;; "\\(:[^[:space:]]*?\\)[[:space:]]*\\(=>[[:space:]]*\\([^[:space:]]*\\)[[:space:]]*\\)?[,){}$]"
  "Regexp to match subsequent key => value pairs of a ruby hash.")

(defun rails-ruby-hash-to-alist ()
  "Returns an alist of the key => value pairs on consecutive
lines starting at point."
  (interactive)
  (let ((end (save-excursion
	       (re-search-forward "[^,{(]$" nil t)
	       (+ 1 (point))))
	alist)
    (save-excursion
      (while (and (< (point) end)
		  (re-search-forward rails-ruby-hash-regexp end t))
	(setf alist
	      (cons (cons
		     (match-string 1)
		     (if (> (length (match-string 3)) 0)
			 (match-string 3)
		       "true"))
		    alist))))
    alist))

(defun rails-find-action (action &optional controller)
  (interactive)
  (if controller
      (find-file (concat (rails-root "app/controllers/" controller ".rb"))))
  (beginning-of-buffer)
  (search-forward-regexp (concat "def\\s" action))
  (recenter))

(defun rails-controller-name-from-view ()
  (concat (rails-root) 
	  "app/controllers/"
	   (file-name-nondirectory 
	    (expand-file-name "."))
	  "_controller.rb"))

;;; TODO: make this work with rails2-style view filenames
(defun rails-find-action ()
  (interactive)
  (let ((action (file-name-sans-extension (file-name-nondirectory buffer-file-name))))
    (find-file (rails-controller-name-from-view))
    (beginning-of-buffer)
    (search-forward-regexp (concat "def *" action))
    (recenter)))

(defun rails-name-components (name)
  "Helper for view toggling"
  (let ((case-fold-search nil))
	(labels ((rnc (in)
			(let ((ind (string-match "\\([A-Z][a-z0-9]+\\)[A-Z]" name in)))
			  (if (eq ind nil)
			      nil
			    (cons (downcase (match-string 1 name)) (rnc (match-end 1)))))))
	  (rnc 0))))

(defun rails-make-dirname (comps)
  "Helper for view toggling"
  (reduce #'(lambda (str next) (concat str (concat "_" next))) comps))

(defun rails-insert-erb-skeleton (no-equals)
  (interactive "P")
  (setq no-e no-equals)
  (insert "<%")
  (unless no-equals (insert "="))
  (insert "  %>")
  (backward-char 3))

(defadvice find-file-in-project (around find-file-in-rails-project activate)
  "Wrap `find-file-in-project' to use `rails-root' as the base of
  the project."
  (let ((ffip-project-root (rails-root)))
    ad-do-it))

;;--------------------------------------------------------------------
;;
;; minor mode and keymaps
;; 
(defvar rinari-minor-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Key map for Rinari minor mode.")
(define-key rinari-minor-mode-map "\C-c's" 'rails-script)
(define-key rinari-minor-mode-map "\C-c'c" 'rails-script-console)
(define-key rinari-minor-mode-map "\C-c'w" 'rails-script-server)
(define-key rinari-minor-mode-map "\C-c'v" 'rails-find-view)
(define-key rinari-minor-mode-map "\C-c't" 'toggle-buffer)

(defun rinari-launch-minor-mode ()
  "Run `rinari-minor-mode' if inside of a rails projcect"
  (interactive)
  (if (rails-root)
      (rinari-minor-mode)))

(add-hook 'find-file-hook
	  (lambda ()
	    (rinari-launch-minor-mode)))

(add-hook 'dired-mode-hook
          (lambda ()
            (rinari-launch-minor-mode)))

(define-minor-mode rinari-minor-mode
  "Enable Rinari minor mode providing Emacs support for working
with the Ruby on Rails framework."
  nil
  " Rinari"
  rinari-minor-mode-map)

(provide 'rinari)
;;; rinari.el ends here