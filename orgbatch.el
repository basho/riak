;; setup erlang mode
(let ((tp (shell-command-to-string "erl -noinput -eval \"io:format(\\\"~s\\\", [code:lib_dir(tools)])\" -run init stop")))
  (setq load-path (cons (concat tp "/emacs")
                        load-path))
  (require 'erlang-start))

;; Org-mode
;; specify ORG_PATH if you have org-mode installed somewhere
;; other than your site-lisp path
(let ((orgpath (getenv "ORG_PATH")))
  (if (stringp orgpath)
      (setq load-path
            (cons (concat orgpath "/lisp")
                  (cons (concat orgpath "/contrib/lisp")
                        load-path)))))
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; allow css to set code highlighting style
(setq org-export-htmlize-output-type 'css)

;; command-line --eval functions
(defun riak-export-doc-file (filename format)
  (find-file filename)
  (cond ((eq format 'ascii) (org-export-as-ascii 3)
         (kill-this-buffer))
        ((eq format 'html) (org-export-as-html 3)
         (kill-this-buffer))
        (message "Unknown export format"))
  (kill-this-buffer))

(defun riak-export-doc-dir (directory format)
  (mapcar
   (lambda (filename) (riak-export-doc-file filename format))
   (file-expand-wildcards (concat directory "/*.org"))))
