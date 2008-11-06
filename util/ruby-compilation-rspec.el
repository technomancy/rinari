;; Add rspec support to ruby-compilation

(require 'ruby-compilation)

(add-hook 'ruby-mode-hook (lambda ()
                            (when (string-match "_spec.rb$" buffer-file-name)
                              (set (make-local-variable 'ruby-compilation-executable)
                                   "spec")
                              (set (make-local-variable 'ruby-compilation-test-name-flag)
                                   "-e"))))

;; Redefining this will break regular test/unit ruby-compilation.
;; Refactor this to use flet so the new definition is buffer-local.
(defun ruby-compilation-this-test-name ()
  "Which test are we currently in?"
  (save-excursion
    (search-backward-regexp "it [\"']\\(.*\\)[\"'] do")
    (match-string 1)))
  
(provide 'ruby-compilation-rspec)