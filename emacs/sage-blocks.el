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
  "Any line beginning with `sage-block-delimiter' is considered a start of a block."
  :type 'string
  :group 'sage-blocks)

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
  (next-line)
  ; search forward: if it worked, move to begin of delimiter, otherwise end of file
  (if (search-forward-regexp (concat "^" sage-block-delimiter) nil 0)
      (goto-char (match-beginning 0))))

(defun sage-send-current-block ()
  "Send the block that the point is currently in to the inferior shell."
  (interactive)
  (if (eq (current-column) 0) ;; handle border-case: standing on block-delimiter
      (next-line))
  (sage-send-region (progn (sage-backward-block) (point))
		    (progn (sage-forward-block)  (point))))

(define-key sage-mode-map (kbd "C-<return>") 'sage-send-current-block)
(define-key sage-mode-map (kbd "M-{")        'sage-backward-block)
(define-key sage-mode-map (kbd "M-}")        'sage-forward-block)


;;
;; Functionality for the inferior shell
;;
(defun sage-pull-next-block ()
  "Evaluate the next block of the last visited file.

TODO: This currently doesn't care what the last file was, even if it doesn't
  contain Sage or Python code.
  "
  (interactive)
  (other-window 1)
  (sage-send-current-block))

(define-key inferior-sage-mode-map (kbd "C-<return>") 'sage-pull-next-block)

(provide 'sage-blocks)
