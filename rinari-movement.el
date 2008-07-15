;;; rinari-movement.el --- functions for quick movement through a Rails project

;; Copyright (C) 2008 Eric Schulte

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

;; providing quick movement between files in a Ruby on Rails project

;; See TODO file in this directory.

;;; Code:
(require 'inflections)

(defvar rinari-subdirs
  '((:model . "app/models/*\.rb$")
    (:controller . "app/controllers/*_controller\.rb$")
    (:view . "app/views/*.rhtml$")
    (:view_two . "app/views/*.html.erb$")
    (:functional . "test/functional/*_controller_test\.rb$")
    (:unit . "test/unit/*_test\.rb$"))
  "Subdirectories of a Rails projects and their contents")

(defvar rinari-subdir-regexps
  (mapcar
   (lambda (pair)
     (replace-regexp-in-string "\*" "\\(.*\\)" (cdr pair) nil t)) rinari-subdirs)
  "Regexps built from `rinari-subdirs'")

(defmacro rinari-find-file ()
  "If `ido-find-file' is defined then use it." ;; (functionp 'ido-find-file)
  `(if (or (equal 'both ido-mode) (equal 'file ido-mode))
       (ido-find-file)
     (call-interactively 'rinari-find-file-helper)))

(defmacro rinari-completing-read (prompt collection)
  "If `ido-mode' is defined then use it for completion."
  `(if (or (equal 'both ido-mode) (equal 'file ido-mode))
       (ido-completing-read ,prompt ,collection)
     (completing-read ,prompt ,collection)))

(defun rinari-find-file-helper (&optional file)
  (interactive "fFile: ")
  (find-file file))

(defun rinari-join-string (lst &optional seperator)
  "The reverse of `split-string'"
  (interactive)
  (mapconcat 'identity lst seperator))

(defun rinari-open (type name)
  (let ((path (substring (concat (rinari-root)
				 (format (replace-regexp-in-string
					  "\*" "%s" (cdr (assoc type rinari-subdirs)) nil t)
					 name)) 0 -1)))
    (and (file-exists-p path) (find-file path))))

(defun rinari-whats-my-type ()
  "Return what section of the rails project is currently being visited."
  (interactive)
  (let ((path (or (buffer-file-name) "")) group)
    (mapcar
     (lambda (pair)
       (if (string-match (replace-regexp-in-string "\*" ".*" (cdr pair) nil t) path)
	   (setf group (car pair))))
     rinari-subdirs) group))

(defun rinari-whats-my-object ()
  "Return the best guess of the name of the rails object of the
current buffer."
  (interactive)
  (let ((path (or (buffer-file-name) "")) object)
    (mapcar
     (lambda (regexp)
       (if (string-match regexp path)
	   (setf object (singularize-string (match-string 1 path)))))
     rinari-subdir-regexps)
    ;; take only before first / in object
    (if (and object (string-match "^\\(.*?\\)/" object)) (match-string 1 object)
      object)))

;;--------------------------------------------------------------------------------
;; Follow renders, redirects, link_to's, and information in ruby keyword arguments

(defvar rinari-ruby-hash-regexp
  "\\(:[^[:space:]]*?\\)[[:space:]]*\\(=>[[:space:]]*[\"\':]?\\([^[:space:]]*?\\)[\"\']?[[:space:]]*\\)?[,){}\n]"
  "Regexp to match subsequent key => value pairs of a ruby hash.")

(defun rinari-get-controller-and-action ()
  "Return the controller and the action from the current buffer and point"
  (let ((path (buffer-file-name)) (method (ruby-add-log-current-method)))
    (cons
     ;; controller
     (and (or (string-match "app/controllers/\\(.*\\)_controller\.rb$" path)
	      (string-match "app/views/\\(.*\\)/.*" path))
	  (match-string 1 path))
     ;; action
     (or (and method (string-match "#\\(.*\\)" method) (match-string 1 method))
	 (and (or (string-match "app/views/.*/_?\\(.*?\\)\.rhtml" path)
		  (string-match "app/views/.*/_?\\(.*?\\)\.html.erb" path))
	      (match-string 1 path))))))

(defun rinari-find-controller-and-action (view-or-controller controller action)
  (let* ((path (rinari-follow-controller-and-action controller action))
	 (controller (car path))
	 (controllers (pluralize-string controller))
	 (action (cdr path)))
    (case view-or-controller
      (:view (or (rinari-open :view (rinari-join-string (list controller action) "/"))
		 (rinari-open :view_two (rinari-join-string (list controller action) "/"))
		 (rinari-open :view (rinari-join-string (list controllers action) "/"))
		 (rinari-open :view_two (rinari-join-string (list controllers action) "/"))
		 (rinari-open :view (rinari-join-string (list controller action) "/_"))
		 (rinari-open :view_two (rinari-join-string (list controller action) "/_"))
		 (rinari-open :view (rinari-join-string (list controllers action) "/_"))
		 (rinari-open :view_two (rinari-join-string (list controllers action) "/_"))
		 (let ((default-directory (concat (rinari-root) "app/views/")))
		   (rinari-find-file))))
      (:controller (rinari-find-action-in-controller controller action)))))

(defun rinari-find-action-in-controller (controller action)
  "Place point on the beginning of the definition of ACTION in CONTROLLER."
  (and (rinari-open :controller controller)
       (goto-char (point-max))
       (re-search-backward (format "def *%s$" action) nil t)))

(defun rinari-ruby-values-from-render (controller action)
  "Adjusts CONTROLLER and ACTION acording to keyword arguments in
the hash at `point', then return (CONTROLLER . ACTION)"
  (let ((end (save-excursion
	       (re-search-forward "[^,{(]$" nil t)
	       (+ 1 (point)))))
    (save-excursion
      (while (and (< (point) end)
		  (re-search-forward rinari-ruby-hash-regexp end t))
	(if (> (length (match-string 3)) 1)
	    (case (intern (match-string 1))
	      (:partial (setf action (concat "_" (match-string 3))))
	      (:action  (setf action (match-string 3)))
	      (:controller (setf controller (match-string 3)))))))
    (cons controller action)))

(defun rinari-which-render (renders)
  (let ((path (rinari-completing-read
	       "Follow: "
	       (mapcar (lambda (lis)
			 (rinari-join-string (list (car lis) (cdr lis)) "/"))
		       renders))))
    (string-match "\\(.*\\)/\\(.*\\)" path)
    (cons (match-string 1 path) (match-string 2 path))))

(defun rinari-follow-controller-and-action (controller action)
  "Follow the current controller-and-action through all of the
renders and redirects to find the final controller or view."
  (save-excursion
    (if (or (rinari-find-action-in-controller controller action)
	    (rinari-find-action-in-controller
	     (setf controller (pluralize-string controller)) action))
	;; there is a controller entry
	(let ((start (point))
	      (renders (list (cons controller action))) render view)
	  (ruby-forward-sexp)
	  ;; collect redirection options and pursue
	  (while (re-search-backward "re\\(?:direct_to\\|nder\\)" start t)
	    (add-to-list 'renders (rinari-ruby-values-from-render controller action)))
	  (let ((render (if (equal 1 (length renders))
			    (car renders)
			  (rinari-which-render renders))))
	    (if (and (equalp (cdr render) action)
		     (equalp (car render) controller))
		(cons controller action) ;; directed to here so return
	      (rinari-follow-controller-and-action (or (car render)
						       controller)
						   (or (cdr render)
						       action)))))
      ;; no controller entry so return
      (cons controller action))))

;;--------------------------------------------------------------------------------
;; movement functions

(defun rinari-find-model ()
  "Go to the most logical model given the current location."
  (interactive)
  (case (rinari-whats-my-type)
    (:unit (toggle-buffer))
    (t (let ((obj (rinari-whats-my-object)))
	 (message (format "%S" obj))
	 (if obj
	     (or (rinari-open :model (singularize-string obj))
		 (rinari-open :model (pluralize-string obj)))
	   (let ((default-directory (concat (rinari-root) "app/models/")))
	     (rinari-find-file)))))))

(defun rinari-find-test ()
  "Go to the most logical test given the current location."
  (interactive)
  (case (rinari-whats-my-type)
    (:model      (toggle-buffer))
    (:controller (toggle-buffer))
    ((:view :view_two) (rinari-find-type :controller) (toggle-buffer))
    (t (let ((default-directory (concat (rinari-root) "test/")))
	 (rinari-find-file)))))

(defun rinari-find-controller ()
  "Go to the most logical controller given the current location."
  (interactive)
  (case (rinari-whats-my-type)
    (:controller (rinari-find-controller-and-action
	    :controller (rinari-whats-my-object)
	    (let ((method (ruby-add-log-current-method)))
	      (and (string-match "#\\(.*\\)" method)
		   (match-string 1 method)))))
    ((:view :view_two) (rinari-find-controller-and-action
	    :controller (rinari-whats-my-object)
	    (let ((file (file-name-nondirectory (buffer-file-name))))
	    (and (or (string-match "\\(.*\\)\.rhtml" file) (string-match "\\(.*\\)\.html.erb" file))
		 (match-string 1 file)))))
    (t (let ((obj (rinari-whats-my-object)))
	 (if obj
	     (or (rinari-open :controller (pluralize-string obj))
		 (rinari-open :controller (singularize-string obj)))
	   (let ((default-directory (concat (rinari-root) "app/controllers/")))
	     (rinari-find-file)))))))

(defun rinari-find-view ()
  "Go to the most logical view given the current location."
  (interactive)
  (case (rinari-whats-my-type)
    (:functional (toggle-buffer) (rinari-find-view))
    (:unit (rinari-find-model) (rinari-find-view))
    (:model
     (let* ((model (rinari-whats-my-object))
	    (default-directory (concat (rinari-root) "app/views/"
				       (pluralize-string model))))
       (rinari-find-file)))
    (:controller 
     (rinari-find-controller-and-action
      :view (rinari-whats-my-object)
      (let ((method (ruby-add-log-current-method)))
	(and (string-match "#\\(.*\\)" method) (match-string 1 method)))))
    ((:view :view_two)
     (rinari-find-controller-and-action
      :view (rinari-whats-my-object)
      (let ((file (file-name-nondirectory (buffer-file-name))))
	(and (or (string-match "\\(.*\\)\.rhtml" file) (string-match "\\(.*\\)\.html.erb" file))
	     (match-string 1 file)))))
    (t (let ((default-directory (concat (rinari-root) "app/views/")))
	 (rinari-find-file)))))

;;--------------------------------------------------------------------------------
;; simple movement functions
(defvar rinari-simple-places
  '(("migration" . "/db/migrate/") ("environment" . "/config/environments/")
    ("javascript" . "/public/javascripts/") ("stylesheet" . "/public/stylesheets/"))
  "Rails directories and the key to find them")

(defmacro rinari-find-simple-place (name path)
  `(defun ,(intern (format "rinari-find-%s" name)) ()
     ,(format "Find a %s in the current Rails project" name)
     (interactive)
     (let ((default-directory (concat (rinari-root) ,path)))
       (rinari-find-file))))

(mapcar (lambda (pair)
	  (eval (list 'rinari-find-simple-place (car pair) (cdr pair))))
	rinari-simple-places)

(provide 'rinari-movement)
;;; rinari-movement.el ends here