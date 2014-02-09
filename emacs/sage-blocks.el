;;; sage-blocks.el --- Support for structuring Sage code in sheets

;; Copyright (C) 2013 Johan S. R. Nielsen

;; Author: Johan S. R. Nielsen <jsrn@jsrn.dk>
;; Keywords: sage

;;; Commentary:

;; This file adds functionality which supports structuring experimental Sage
;; code in "sheets", where the code is bundled in "blocks" akin to the boxes of
;; the Notebook. The core is concerned with convenient handling of such
;; blocks. The file injects a few keybindings into `sage-mode' as well as
;; `inferior-sage-mode'.

;; A block is defined by a line beginning with `sage-block-delimiter'.

;;; Code:

(defcustom sage-block-delimiter "###"
  "Any line matching the regular expression `sage-block-delimiter' at the
  beginning of the line is considered a start of a block.

Note that '^' to match at the beginning of the line should not be added to
`sage-block-delimiter'.
Strange behaviour might arise if `sage-block-delimiter' matches multiple lines
at a time."
  :type 'string
  :group 'sage)

;;
;; Functionality for Sage source files
;;
(defun sage-backward-block ()
  "Move backwards to the last beginning of a block."
  (interactive)
  (search-backward-regexp (concat "^" sage-block-delimiter) nil 0))

(defun sage-forward-block ()
  "Move forwards to the next beginning of a block."
  (interactive)
  ; If point is on a delimiter, we should skip this, so search from beginning of
  ; next line (this will match immediately, if next line is a delimiter)
  (forward-line)
  ; search forward: if it worked, move to begin of delimiter, otherwise end of file
  (when (search-forward-regexp (concat "^" sage-block-delimiter) nil 0)
      (goto-char (match-beginning 0))))

(defun sage-send-current-block ()
  "Send the block that the point is currently in to the inferior shell."
  (interactive)
  ;; Border-case: if we're standing on a delimiter, sage-backward-block will go
  ;; to previous delimiter, but we should send from this delimiter and forwards.
  (beginning-of-line)
  (let ((backdelim (progn
		     (unless (looking-at (concat "^" sage-block-delimiter))
		      (sage-backward-block))
		     (point))))
    (sage-send-region backdelim (progn (sage-forward-block)  (point)))))

(define-key sage-mode-map (kbd "C-<return>") 'sage-send-current-block)
(define-key sage-mode-map (kbd "M-{")        'sage-backward-block)
(define-key sage-mode-map (kbd "M-}")        'sage-forward-block)


;;
;; Functionality for the inferior shell
;;
(defun sage-pull-next-block ()
  "Evaluate the next block of the last visited file in Sage mode."
  (interactive)
  ;; Find the first buffer in buffer-list which is in sage-mode
  (let ((buf
	 (progn
	   (setq lst (buffer-list))
	   (catch 'break
	     (while lst
	       (if (with-current-buffer (car lst) (derived-mode-p 'sage-mode))
		   (throw 'break (car lst))
		 (setq lst (cdr lst))))))
	 ))
    (if buf
	(progn
	  (switch-to-buffer-other-window buf)
	  (sage-send-current-block))
      (error "No sage-mode buffer found"))))

(define-key inferior-sage-mode-map (kbd "C-<return>") 'sage-pull-next-block)

(provide 'sage-blocks)

;;; sage-blocks.el ends here
