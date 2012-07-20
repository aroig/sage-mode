;;; sage-auctex.el --- AUCTeX support for Sage code

;; Copyright (C) 2012  Ivan Andrus

;; Author: Ivan Andrus <darthandrus@gmail.com>
;; Keywords: sage tex

;;; Commentary:

;; This file adds functionality to AUCTeX supporting usage of SageTeX.

;;; Code:

(eval-when-compile (and (require 'tex-site nil t)
			(require 'tex)
			(require 'tex-buf)))

;;;###autoload
(defun sage-run-sagetex (name command file)
  "Function to be used in `TeX-command-list' to run sage on sagetex files.
Makes `TeX-command-default' the next command to be run since we
need to retypeset after running sage.
For details of NAME, COMMAND and FILE see the documetation for `TeX-command-list'."
  (TeX-run-compile name command file)
  ;; TODO: This only works if there is a process buffer, i.e. not a compile buffer.
  (with-no-warnings
    (TeX-process-set-variable file 'TeX-command-next
			      TeX-command-default)))

(defadvice TeX-LaTeX-sentinel (after LaTeX-recognize-sage (process name))
  "Recognize when Sage needs to be run."
  ;; Only run if we haven't already found something "more important" to do
  (when (eq TeX-command-next TeX-command-Show)
    (cond ((re-search-forward "Package sagetex Warning: `final' option provided" nil t)
           (message "Remove the final option to sagetex and rerun (La)TeX." )
           (setq TeX-command-next TeX-command-default))
          ;; More general -- right now there is only one warning
          ((re-search-forward "Package sagetex Warning: \\(.*\\)" nil t)
           (message "You should run Sage: %s." (match-string 1))
           (setq TeX-command-next "Sage")))))

;;;###autoload
(defun sage-auctex-setup ()
  "Hooks Sage support into AUCTeX.
Must be called after AUCTeX has been loaded.

Adds entries `TeX-expand-list' and `TeX-command-list' and advises
`TeX-LaTeX-sentinel'."
  (add-to-list 'TeX-expand-list
               '("%(sage)" (lambda () (require 'sage) sage-command)))
  (add-to-list 'TeX-expand-list
               '("%(sagetex-extension)" (lambda () ".sagetex.sage")))
  (add-to-list 'TeX-command-list
               '("Sage" "%(sage) %s%(sagetex-extension)"
                 sage-run-sagetex t t
                 :help "Run Sage on sagetex files."))
  ;; Support the Clean command
  (dolist (l '(TeX-clean-default-intermediate-suffixes
               plain-TeX-clean-intermediate-suffixes
               LaTeX-clean-intermediate-suffixes
               ConTeXt-clean-intermediate-suffixes
               docTeX-clean-intermediate-suffixes
               Texinfo-clean-output-suffixes))
    ;; If they are not bound they will pick up the value from
    ;; `TeX-clean-default-intermediate-suffixes'
    (when (boundp l)
      ;; cleanup .sagetex.sage .sagetex.py .sagetex.sout .sagetex.cmd
      (add-to-list l "\\.sagetex\\..*")))

  (ad-enable-advice 'TeX-LaTeX-sentinel 'after 'LaTeX-recognize-sage)
  (ad-activate 'TeX-LaTeX-sentinel))

;;;###autoload
(eval-after-load 'tex '(sage-auctex-setup))

(provide 'sage-auctex)

;;; sage-auctex.el ends here
