;;; sage-compat.el --- Compatibility with new python.el

;; Hack around new python.el.  Eventually all of this should go away,
;; but for now it's the easiest way to get things working.

;;; Commentary:

;;; Code:

(require 'python)
(require 'rx)

;; From org-called-interactively-p
(defmacro sage-called-interactively-p (&optional kind)
  (if (featurep 'xemacs)
      `(interactive-p)
    (if (or (> emacs-major-version 23)
	    (and (>= emacs-major-version 23)
		 (>= emacs-minor-version 2)))
	`(with-no-warnings (called-interactively-p ,kind)) ;; defined with no argument in <=23.1
      `(interactive-p))))

(eval-when-compile
  (unless (fboundp 'help-print-return-message)
    (defalias 'help-print-return-message 'print-help-return-message)))

(eval-when-compile
  (if (fboundp 'python-beginning-of-string)
      (defalias 'sage-beginning-of-string 'python-beginning-of-string)
    (with-no-warnings
      (defun sage-beginning-of-string ()
	"Go to beginning of string around point.
Do nothing if not in string."
	(let ((bos (python-info-ppss-context 'string)))
	  (when bos
	    (goto-char bos)))))))

(eval-when-compile
  (if (fboundp 'python-in-string/comment)
      (defalias 'sage-in-string/comment 'python-in-string/comment)
    (defalias 'sage-in-string/comment 'python-info-ppss-comment-or-string-p)))

(eval-when-compile
  (unless (boundp 'python-prev-dir/file)
    (defvar python-prev-dir/file nil)))

(eval-when-compile
  (unless (fboundp 'python-comment-line-p)
    (defalias 'python-comment-line-p 'python-info-current-line-comment-p)))

(eval-when-compile
  (unless (fboundp 'python-beginning-of-statement)
    (defalias 'python-beginning-of-statement 'python-nav-beginning-of-statement)))

(eval-when-compile
  (unless (fboundp 'python-end-of-statement)
    (defalias 'python-end-of-statement 'python-nav-end-of-statement)))

(eval-when-compile
  (unless (fboundp 'python-comment-line-p)
    (defalias 'python-comment-line-p 'python-info-current-line-comment-p)))

(eval-when-compile
  (unless (fboundp 'python-open-block-statement-p)
    (defalias 'python-open-block-statement-p 'python-info-beginning-of-block-p)))

(eval-when-compile
  (unless (fboundp 'python-previous-statement)
    (defalias 'python-previous-statement #'python-nav-backward-sentence)))

(eval-when-compile
  (unless (fboundp 'python-beginning-of-block)
    (defalias 'python-beginning-of-block #'python-nav-beginning-of-block)))

(eval-when-compile
  (unless (fboundp 'python-end-of-block)
    (defalias 'python-end-of-block 'python-nav-end-of-block)))

;; Changed pyrex to cython
(define-obsolete-function-alias 'pyrex-mode 'cython-mode "0.10")
(define-obsolete-function-alias 'pyrex-mode-p 'cython-mode-p "0.10")
(define-obsolete-function-alias 'pyrex-open-block-statement-p 'cython-open-block-statement-p "0.10")
(define-obsolete-function-alias 'pyrex-beginning-of-defun 'cython-beginning-of-defun "0.10")
(define-obsolete-function-alias 'pyrex-end-of-defun 'cython-end-of-defun "0.10")
(define-obsolete-function-alias 'pyrex-current-defun 'cython-current-defun "0.10")

;; Changes to python.el caused sage to freeze because it wasn't
;; looking for the correct prompt
(when (boundp 'python-shell-prompt-input-regexps)
  (add-to-list 'python-shell-prompt-input-regexps "sage: ")
  (add-to-list 'python-shell-prompt-input-regexps "\\.\\.\\.\\.: ")
  (python-shell-prompt-set-calculated-regexps))

(when (string-match "24\\.4\\.[0-9.]*" emacs-version)
  (defun python-shell-get-buffer ()
    "Return inferior Python buffer for current buffer.
If current buffer is in `inferior-python-mode', return it."
    (if (derived-mode-p 'inferior-python-mode)
	(current-buffer)
      (let* ((dedicated-proc-name (python-shell-get-process-name t))
	     (dedicated-proc-buffer-name (format "*%s*" dedicated-proc-name))
	     (global-proc-name  (python-shell-get-process-name nil))
	     (global-proc-buffer-name (format "*%s*" global-proc-name))
	     (dedicated-running (comint-check-proc dedicated-proc-buffer-name))
	     (global-running (comint-check-proc global-proc-buffer-name)))
	;; Always prefer dedicated
	(or (and dedicated-running dedicated-proc-buffer-name)
	    (and global-running global-proc-buffer-name))))))


(provide 'sage-compat)

;;; sage-compat.el ends here
