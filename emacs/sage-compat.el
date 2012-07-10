;;;_* sage-compat.el --- Compatibility with new python.el

;; Hack around new python.el.  Eventually all of this should go away,
;; but for now it's the easiest way to get things working.

;;; Commentary:

;; Much of this is stolen from python.el in Emacs 22.1.1

;;; Code:

(require 'python)
(require 'rx)

(when (not (fboundp 'python-comment-line-p))
  (defun python-comment-line-p ()
    "Return non-nil iff current line has only a comment."
    (save-excursion
      (end-of-line)
      (when (eq 'comment (syntax-ppss-context (syntax-ppss)))
	(back-to-indentation)
	(looking-at (rx (or (syntax comment-start) line-end)))))))

(when (not (fboundp 'python-beginning-of-statement))
  (defun python-beginning-of-statement ()
    "Huge hack."
    (interactive)
    (python-nav-forward-sentence)
    (python-nav-backward-sentence)))

(when (not (fboundp 'python-comment-line-p))
  (defun python-comment-line-p ()
    "Return non-nil iff current line has only a comment."
    (save-excursion
      (end-of-line)
      (when (python-info-ppss-context 'comment (syntax-ppss))
	(back-to-indentation)
	(looking-at (rx (or (syntax comment-start) line-end)))))))

(when (not (fboundp 'python-open-block-statement-p))
  (defun python-open-block-statement-p (&optional bos)
    "Return non-nil if statement at point opens a block.
BOS non-nil means point is known to be at beginning of statement."
    (save-excursion
      (unless bos (python-beginning-of-statement))
      (looking-at (rx (and (or "if" "else" "elif" "while" "for" "def"
			       "class" "try" "except" "finally" "with")
			   symbol-end))))))

(when (not (fboundp 'python-previous-statement))
  (defalias 'python-previous-statement #'python-nav-backward-sentence))

(when (not (fboundp 'python-blank-line-p))
  (defun python-blank-line-p ()
    "Return non-nil iff current line is blank."
    (save-excursion
      (beginning-of-line)
      (looking-at "\\s-*$"))))

;; I'm not sure this is correct, but at least it's a first attempt
(when (not (fboundp 'python-beginning-of-block))
  (defun python-beginning-of-block (&optional arg)
    "Pathetic!!"
    (interactive "p")
    (unless arg (setq arg 1))
    (cond
     ((< arg 0)
      (python-end-of-block (- arg)))
     ((zerop arg))
     (t
      (let ((point (point)))

	(if (or (python-comment-line-p)
		(python-blank-line-p))
	    (python-skip-comments/blanks t))

	(python-beginning-of-statement)
	(let ((ci (current-indentation)))
	  (if (zerop ci)
	      (not (goto-char point))     ; return nil
	    ;; Look upwards for less indented statement.
	    (if (catch 'done
;;; This is slower than the below.
		  (while (progn (python-previous-statement) t)
		    (when (and (< (current-indentation) ci)
			       (python-open-block-statement-p t))
		      (beginning-of-line)
		      (throw 'done t)))
		  (not (goto-char point))) ; Failed -- return nil
		(python-beginning-of-block (1- arg))))))))))

(when (not (boundp 'python-space-backslash-table))
  (defconst python-space-backslash-table
    (let ((table (copy-syntax-table python-mode-syntax-table)))
      (modify-syntax-entry ?\\ " " table)
      table)
    "`python-mode-syntax-table' with backslash given whitespace syntax."))

(when (not (fboundp 'python-skip-comments/blanks))
  (defun python-skip-comments/blanks (&optional backward)
    "Skip comments and blank lines.
BACKWARD non-nil means go backwards, otherwise go forwards.
Backslash is treated as whitespace so that continued blank lines
are skipped.  Doesn't move out of comments -- should be outside
or at end of line."
    (let ((arg (if backward
		   ;; If we're in a comment (including on the trailing
		   ;; newline), forward-comment doesn't move backwards out
		   ;; of it.  Don't set the syntax table round this bit!
		   (let ((syntax (syntax-ppss)))
		     (if (nth 4 syntax)
			 (goto-char (nth 8 syntax)))
		     (- (point-max)))
		 (point-max))))
      (with-syntax-table python-space-backslash-table
	(forward-comment arg)))))

(when (not (fboundp 'python-end-of-block))
  (defun python-end-of-block (&optional arg)
    "Go to end of current block.
With numeric arg, do it that many times.  If ARG is negative,
call `python-beginning-of-block' instead.
If current statement is in column zero and doesn't open a block,
don't move and return nil.  Otherwise return t."
    (interactive "p")
    (unless arg (setq arg 1))
    (if (< arg 0)
	(python-beginning-of-block (- arg))
      (while (and (> arg 0)
		  (let* ((point (point))
			 (_ (if (python-comment-line-p)
				(python-skip-comments/blanks t)))
			 (ci (current-indentation))
			 (open (python-open-block-statement-p))
			 opoint)
		    (if (and (zerop ci) (not open))
			(not (goto-char point))
		      (catch 'done
			(setq opoint (point))
			(while (progn (python-nav-forward-sentence)
				      (not (= opoint (point))))
			  (setq opoint (point))
			  (when (or (and open (<= (current-indentation) ci))
				    (< (current-indentation) ci))
			    (python-skip-comments/blanks t)
			    (beginning-of-line 2)
			    (throw 'done t)))))))
	(setq arg (1- arg)))
      (zerop arg)))
  )

(provide 'sage-compat)

;;; sage-mode.el ends here
