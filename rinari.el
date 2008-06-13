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
(require 'ruby-mode)
(require 'inf-ruby)
(require 'toggle)

(require 'find-file-in-project)
(require 'pcmpl-rake)
(require 'rails-scripts)

;;;###autoload
(defun rake (task)
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
 	 (cls (rails-make-dirname (rails-name-components funname)))
	 (fn (and (string-match "#\\(.*\\)" funname) (match-string 1 funname)))
 	 (appdir (file-name-directory (directory-file-name (file-name-directory (buffer-file-name))))))
    (find-file (concat appdir "views/" cls "/" fn ".rhtml"))))

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

;; keymaps
(define-key ruby-mode-map (kbd "C-c C-v") 'rails-find-view)
(define-key ruby-mode-map (kbd "C-c C-t") 'toggle-buffer)
(define-key ruby-mode-map (kbd "C-c C-M-t") 'ruby-test-file)
(define-key ruby-mode-map (kbd "C-c C-S-t") 'ruby-test-one)
;;
;; some temporary keymaps (will replace when have minor-mode)
;; 
;; ("Key Binding Conventions" node of the Elisp manual)
(define-key ruby-mode-map (kbd "C-c v s") 'rails-script)
(define-key ruby-mode-map (kbd "C-c v w") 'rails-script-server)
(define-key ruby-mode-map (kbd "C-c v c") 'rails-script-console)

;; nxhtml-mode is the cats!
(eval-after-load 'nxhtml-mode
  '(progn
     (define-key nxhtml-mode-map (kbd "C-c C-v") 'rails-find-action)
     (define-key nxhtml-mode-map (kbd "C-c C-e") 'rails-insert-erb-skeleton)))

;; nxhtml stuff
(setq mumamo-chunk-coloring 'submode-colored
      nxhtml-skip-welcome t
      rng-nxml-auto-validate-flag nil)

(provide 'rinari)
;;; rinari.el ends here