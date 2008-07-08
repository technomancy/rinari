(cd (file-name-directory (or load-file-name buffer-file-name)))
(add-to-list 'load-path "..")

(require 'rinari)
(require 'elunit)

;; testing rinari-movement
(defsuite rinari-movement-suite nil
  ;; :setup-hook (lambda () )
  :teardown-hook (lambda () (message "done testing")))

(deftest rinari-move-test rinari-movement-suite
  ;; test moving from everywhere to everywhere
  (save-excursion
    (let ((max-lisp-eval-depth 2000)
	  (rails-root (format "%s" (concat (file-name-directory (buffer-file-name)) "rails-app/"))))
      (message rails-root)
      (flet ((here-to-here (start func end)
			   (let ((default-directory rails-root))
			     ;; go to start
			     (find-file (car start))
			     (goto-char (cdr start))
			     ;; run movement function
			     (eval (list func))
			     ;; assert
			     (assert-equal (file-name-nondirectory (car end))
					   (file-name-nondirectory (buffer-file-name)))
			     (assert-equal (cdr end) (point))
			     ;; clean up
			     (kill-buffer (file-name-nondirectory (car end)))
			     (kill-buffer (file-name-nondirectory (car start))))))
	(here-to-here '("test/unit/example_test.rb" . 153)
		      'rinari-go-to-model
		      '("app/models/example.rb" . 39))
	(here-to-here '("app/controllers/units_controller.rb" . 52)
		      'rinari-go-to-test
		      '("test/functional/units_controller_test.rb" . 152))
	(here-to-here '("app/controllers/units_controller.rb" . 61)
		      'rinari-go-to-view
		      '("app/views/units/fall.rhtml" . 1))))))
