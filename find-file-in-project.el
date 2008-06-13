;;; find-file-in-project.el --- Find files in a project quickly.

;; Copyright (C) 2006, 2007, 2008 Phil Hagelberg and Doug Alcorn

;; Author: Phil Hagelberg and Doug Alcorn
;; URL: http://www.emacswiki.org/cgi-bin/wiki/FindFileInProject
;; Version: 2.0
;; Created: 2008-03-18
;; Keywords: project, convenience
;; EmacsWiki: FindFileInProject

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

;; This file provides a method for quickly finding any file in a given
;; project. Projects are defined as per the `project-local-variables'
;; library, by the presence of a `.emacs-project' file in a directory.

;; By default, it looks only for files whose names match
;; `ffip-regexp', but it's understood that that variable will be
;; overridden locally. This can be done either with a mode hook:

;; (add-hook 'emacs-lisp-mode-hook (lambda (setl ffip-regexp ".*\\.el")))

;; or by setting it in your .emacs-project file, in which case it will
;; get set locally by the project-local-variables library.

;; You can also be a bit more specific about what files you want to
;; find. For instance, in a Ruby on Rails project, you may be
;; interested in all .rb files that don't exist in the "vendor"
;; directory. In that case you could locally set `ffip-find-options'
;; to "" from within a hook or your .emacs-project file. The options
;; accepted in that variable are passed directly to the Unix `find'
;; command, so any valid arguments for that program are acceptable.

;; If `ido-mode' is enabled, the menu will use `ido-completing-read'
;; instead of `completing-read'.

;; Recommended binding:
;; (global-set-key (kbd "C-x C-M-f") 'find-file-in-project)

;;; TODO:

;; Performance testing with large projects
;; Switch to using a hash table if it's too slow

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; find-file-in-project

(defvar rinari-project-files-table ())

(defvar ffip-subdirectories
  '("app" "config" "db" "lib" "public/javascripts" "public/stylesheets"))

(defun populate-project-files-table (file)
  (if (file-directory-p file)
      (mapc 'populate-project-files-table (directory-files file t "^[^\.]"))
    (let* ((file-name (file-name-nondirectory file))
	   (existing-record (assoc file-name project-files-table))
	   (unique-parts (get-unique-directory-names file (cdr existing-record))))
      (if existing-record
	  (let ((new-key (concat file-name " - " (car unique-parts)))
		(old-key (concat (car existing-record) " - " (cadr unique-parts))))
	    (setf (car existing-record) old-key)
	    (setq project-files-table (acons new-key file project-files-table)))
	(setq project-files-table (acons file-name file project-files-table))))))

(defun get-unique-directory-names (path1 path2)
  (let* ((parts1 (and path1 (split-string path1 "/" t)))
	 (parts2 (and path2 (split-string path2 "/" t)))
	 (part1 (pop parts1))
	 (part2 (pop parts2))
	 (looping t))
    (while (and part1 part2 looping)
	   (if (equal part1 part2)
	       (setq part1 (pop parts1) part2 (pop parts2))
	     (setq looping nil)))
    (list part1 part2)))

(defun find-file-in-project (file)
  (interactive (list (if (or (equalp ido-mode 'file)
			     (equalp ido-mode 'both))
			 (ido-completing-read "Find file in project: "
					      (mapcar 'car (project-files)))
			 (completing-read "Find file in project: "
					  (mapcar 'car (project-files))))))
  (find-file (cdr (assoc file project-files-table))))

;; TODO: revert to the .emacs-project version with a defadvice wrapper
;;       for rails-root
(defun project-files (&optional file)
; uncomment these lines if it's too slow to load the whole project-files-table
;  (when (or (not project-files-table) ; initial load
;	    (not (string-match (rails-root) (cdar project-files-table)))) ; switched projects
    (setq project-files-table nil)
    (if file
	(populate-project-files-table file)
      (let ((root (rails-root)))
	(mapc (lambda (dir)
		(populate-project-files-table (concat root "/" dir)))
	      ffip-subdirectories)))
    project-files-table)

(provide 'find-file-in-project)
;;; find-file-in-project.el ends here
