;;; sage-mode.el --- Major mode for editing SAGE code

;; Copyright (C) 2007  Nick Alexander

;; Author: Nick Alexander <ncalexander@gmail.com>
;; Keywords: sage ipython python math

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'python)
(require 'ansi-color)

;;; Inferior SAGE major mode

(define-derived-mode
  inferior-sage-mode
  inferior-python-mode
  "Inferior SAGE"
  "Major mode for interacting with an inferior SAGE process."

  ; (setq sage-command (expand-file-name "~/bin/sage"))
  (setq comint-prompt-regexp
	(rx line-start (1+ (and (or "sage:" "....." ">>>" "...") " "))))
  (setq comint-redirect-finished-regexp "sage:") ; comint-prompt-regexp)
  ; ansi color doesn't play well with redirect
  ; (ansi-color-for-comint-mode-on)
  ; XXX what should be done here?
  ; (setq python-buffer sage-buffer)

  (define-key inferior-sage-mode-map
    [(control h) (control f)] 'ipython-describe-symbol)
)

(defcustom sage-command (expand-file-name "~/bin/sage")
  "Actual command used to run SAGE.
Additional arguments are added when the command is used by `run-sage' et al."
  :group 'sage
  :type 'string)

(defvar sage-buffer nil
  "*The current SAGE process buffer.

Commands that send text from source buffers to SAGE processes have
to choose a process to send to.  This is determined by buffer-local
value of `sage-buffer'.  If its value in the current buffer,
i.e. both any local value and the default one, is nil, `run-sage'
and commands that send to the Python process will start a new process.

Whenever \\[run-sage] starts a new process, it resets the default
value of `sage-buffer' to be the new process's buffer and sets the
buffer-local value similarly if the current buffer is in SAGE mode
or Inferior SAGE mode, so that source buffer stays associated with a
specific sub-process.

Use \\[sage-set-proc] to set the default value from a buffer with a
local value.")
(make-variable-buffer-local 'sage-buffer)

;;;###autoload
(defun run-sage (&optional cmd noshow new)
  "Run an inferior SAGE process, input and output via buffer *SAGE*.
CMD is the SAGE command to run.  NOSHOW non-nil means don't show the
buffer automatically.

Normally, if there is a process already running in `sage-buffer',
switch to that buffer.  Interactively, a prefix arg allows you to edit
the initial command line (default is `sage-command'); `-i' etc. args
will be added to this as appropriate.  A new process is started if:
one isn't running attached to `sage-buffer', or interactively the
default `sage-command', or argument NEW is non-nil.  See also the
documentation for `sage-buffer'.

Runs the hook `inferior-sage-mode-hook' \(after the
`comint-mode-hook' is run).  \(Type \\[describe-mode] in the process
buffer for a list of commands.)"
  (interactive (if current-prefix-arg
		   (list (read-string "Run SAGE: " sage-command) nil t)
		 (list sage-command)))
  (unless cmd (setq cmd sage-command))
  (setq sage-command cmd)
  ;; Fixme: Consider making `sage-buffer' buffer-local as a buffer
  ;; (not a name) in SAGE buffers from which `run-sage' &c is
  ;; invoked.  Would support multiple processes better.
  (let ((create-new-sage-p
	 (or new			; if you ask for it
	     (null sage-buffer)		; or there isn't a running sage
	     (not (comint-check-proc sage-buffer)) ; or there is a sage
					; buffer, but it's dead
	     )))
    (when create-new-sage-p
      (with-current-buffer
	  (let* ((cmdlist (python-args-to-list cmd))
		 ;; Set PYTHONPATH to import module emacs from emacs.py,
		 ;; but ensure that a user specified PYTHONPATH will
		 ;; override our setting, so that emacs.py can be
		 ;; customized easily.
		 (orig-path (getenv "PYTHONPATH"))
		 (path-sep (if (and orig-path (length orig-path)) ":" ""))
		 (data-path (concat "PYTHONPATH=" orig-path path-sep data-directory))
		 (process-environment
		  (cons data-path process-environment)))
	    (apply 'make-comint-in-buffer "SAGE"
		   (if new (generate-new-buffer "*SAGE*") "*SAGE*")
		   (car cmdlist) nil (cdr cmdlist)))
	;; Show progress
	(unless noshow (pop-to-buffer (current-buffer)))
	;; Update default SAGE buffers
	(setq-default sage-buffer (current-buffer))
	;; Update python-buffer too, so that evaluation keys work
	(setq-default python-buffer (current-buffer))
	;; Set up sensible prompt defaults, etc
	(inferior-sage-mode)
	(accept-process-output (get-buffer-process sage-buffer) 5)
	;; Ensure we're at a prompt before loading the functions we use
	;; XXX error-checking
	(python-send-receive-multiline "import emacs"))))

  ;; If we're coming from a sage-mode buffer, update inferior buffer
  (when (derived-mode-p 'sage-mode)
      (setq sage-buffer (default-value 'sage-buffer)) ; buffer-local
      ;; Update python-buffer too, so that evaluation keys work
      (setq python-buffer (default-value 'sage-buffer))) ; buffer-local

  ;; No matter how we got here, we want this inferior buffer to be the master
  ;; (when (comint-check-proc sage-buffer)
  ;;  (setq-default sage-buffer sage-buffer)
  ;;  (setq-default python-buffer sage-buffer))
  ;; Without this, help output goes into the inferior python buffer if
  ;; the process isn't already running.
  (sit-for 1 t)        ;Should we use accept-process-output instead?  --Stef
  (unless noshow (pop-to-buffer sage-buffer)))

;;; SAGE major mode

(provide 'sage)

(define-derived-mode
  sage-mode
  python-mode
  "SAGE"
  "Major mode for editing SAGE files."

  (define-key sage-mode-map [(control h) (control f)] 'ipython-describe-symbol)
)

;;;###autoload
(add-to-list 'interpreter-mode-alist '("sage" . sage-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.sage\\'" . sage-mode))

(defun python-qualified-module-name (file)
  "Find the qualified module name for filename FILE.

This recurses down the directory tree as long as there are __init__.py
files there, signalling that we are inside a package.

Returns a list of two elements.  The first is the top level package
directory; the second is the dotted Python module name.

Adapted from a patch posted to the python-mode trac."
  (let ((rec #'(lambda (d f)
		 (let* ((dir (file-name-directory d))
			(initpy (concat dir "__init__.py")))
		   (if (file-exists-p initpy)
		       (let ((d2 (directory-file-name d)))
			 (funcall rec (file-name-directory d2)
				  (concat (file-name-nondirectory d2) "." f)))
		     (list d f))))))
    (funcall rec (file-name-directory file)
	     (file-name-sans-extension (file-name-nondirectory file))))) 

;;; Replace original `python-load-file' to use xreload and packages.
(ad-activate 'python-load-file)
(defadvice python-load-file
  (around nca-python-load-file first (file-name &optional xreload))
  "Load a Python file FILE-NAME into the inferior Python process.

THIS REPLACES THE ORIGINAL `python-load-file'.

If the file has extension `.py' import or reload it as a module.
Treating it as a module keeps the global namespace clean, provides
function location information for debugging, and supports users of
module-qualified names."
  (interactive
   (append (comint-get-source "Load Python file: " python-prev-dir/file
					   python-source-modes
					   t)
	   current-prefix-arg))	; because execfile needs exact name
  (comint-check-source file-name)     ; Check to see if buffer needs saving.
  (setq python-prev-dir/file (cons (file-name-directory file-name)
				   (file-name-nondirectory file-name)))
  (with-current-buffer (process-buffer (python-proc)) ;Runs python if needed.
    ;; Fixme: I'm not convinced by this logic from python-mode.el.
    (python-send-command
     (if (string-match "\\.py\\'" file-name)
	 (let* ((directory-module (python-qualified-module-name file-name))
		(directory (car directory-module))
		(module (cdr directory-module))
		(xreload-flag (if xreload "True" "False")))
	   (format "emacs.eimport(%S, %S, use_xreload=%s)"
		   module directory xreload-flag))
       (format "execfile(%S)" file-name)))
    (message "%s loaded" file-name)))

;;; Treat sage code as python source code
(add-to-list 'python-source-modes 'sage-mode)

(defun python-send-receive-multiline (command)
  "Send COMMAND to inferior Python (if any) and return result as a string.
This is an alternate `python-send-receive' that uses temporary buffers and
`comint-redirect-send-command-to-process'.
This implementation handles multi-line output strings gracefully.  At this
time, it does not handle multi-line input strings at all."
  (interactive "sCommand: ")
  (with-temp-buffer
    ;; Grab what Python has to say
    (comint-redirect-send-command-to-process
     command (current-buffer) (python-proc) nil t)
    ;; Wait for the redirection to complete
    (with-current-buffer (process-buffer (python-proc))
      (while (null comint-redirect-completed)
	(accept-process-output nil 1)))
    ;; Return the output
    (let ((output (buffer-substring-no-properties (point-min) (point-max))))
      (when (interactive-p)
	(message output))
      output)))

;;; completing-read for symbols using IPython's *? mechanism
(defvar ipython-completing-read-symbol-history ()
  "List of Python symbols recently queried.")

(defvar ipython-completing-read-symbol-pred nil
  "Predicate for filtering queried Python symbols.")

(defvar ipython-completing-read-symbol-command "%s*?"
  "IPython command for generating completions.
Each completion should appear separated by whitespace.")

(defvar ipython-completing-read-symbol-cache ()
  "A list (last-queried-string string-completions).")

(defun ipython-completing-read-symbol-clear-cache ()
  "Clear the IPython completing read cache."
  (interactive)
  (setq ipython-completing-read-symbol-cache ()))

(defun ipython-completing-read-symbol-make-completions (string)
  "Query IPython for completions of STRING.
Return a list of completion strings.
Uses `ipython-completing-read-symbol-command' to query IPython."
  (let* ((command (format ipython-completing-read-symbol-command string))
	 (output (python-send-receive-multiline command)))
    (condition-case ()
	(split-string output)
      (error nil))))

(defun ipython-completing-read-symbol-function (string predicate action)
  "A `completing-read' programmable completion function for querying IPython.
See `try-completion' and `all-completions' for interface details."
  (let ((cached-string (first ipython-completing-read-symbol-cache))
	(completions (second ipython-completing-read-symbol-cache)))
    ;; Recompute table using IPython if neccessary
    (when (or (null completions)
	      (not (equal string cached-string)))
      (setq ipython-completing-read-symbol-cache
	    (list string (ipython-completing-read-symbol-make-completions string)))
      (setq completions (second ipython-completing-read-symbol-cache)))
    ;; Complete as necessary
    (if action
	(let ((all (all-completions string completions predicate)))
	  (if (eq action 'lambda)
	    (member string all)		; action is lambda
	    all))			; action is t
      (try-completion string completions predicate) ; action is nil
)))

(defun ipython-completing-read-symbol (default)
  "Read a Python symbol from user, using IPython for completion."
  (let ((prompt (if (null default) "IPython symbol: "
		  (format "IPython symbol (default %s): " default)))
	(func 'ipython-completing-read-symbol-function)
	(pred ipython-completing-read-symbol-pred)
	(hist 'ipython-completing-read-symbol-history))
    (ipython-completing-read-symbol-clear-cache)
    (completing-read prompt func pred nil nil hist default)))

;;; `find-function' and `find-variable' for python symbols using IPython's ?
;;; magic mechanism
(defvar ipython-symbol-not-found "Object `.*?` not found."
  "Regexp that matches IPython's 'symbol not found' warning.")

(defvar ipython-symbol-describing-buffer-name "*IPython Symbol*"
  "Temporary buffer for describing symbols.")

(defun ipython-get-symbol-describing-buffer ()
  "Return a temporary buffer.  Create one if necessary."
  (let ((buf (get-buffer-create ipython-symbol-describing-buffer-name)))
    (set-buffer buf)
    buf))

(defun ipython-describe-symbol (symbol)
  "Get help on SYMBOL using IPython's inspection (?).
Interactively, prompt for SYMBOL."
  ;; Note that we do this in the inferior process, not a separate one, to
  ;; ensure the environment is appropriate.
  (interactive
   (let ((symbol (with-syntax-table python-dotty-syntax-table
		   (current-word)))
	 (enable-recursive-minibuffers t))
     (list (ipython-completing-read-symbol symbol))))
  (if (equal symbol "") (error "No symbol"))
  ;; Try to handle symbol not found gracefully  
  (save-excursion
    ;; Clean slate
    (set-buffer (ipython-get-symbol-describing-buffer))
    (delete-region (point-min) (point-max))
    ;; Grab what IPython has to say
    (comint-redirect-send-command-to-process
     (format "%s?" symbol)
     (buffer-name (ipython-get-symbol-describing-buffer)) (python-proc) nil t)
    ;; Wait for the process to complete
    (set-buffer (process-buffer (python-proc)))
    (while (null comint-redirect-completed)
      (accept-process-output nil 1))
    ;; When looking at symbol not found, say so
    (set-buffer (ipython-get-symbol-describing-buffer))
    (goto-char (point-min))
    (when (looking-at ipython-symbol-not-found)
      (error "Symbol `%s' not found" symbol))
    ;; Ensure we have a suitable help buffer.
    ;; Fixme: Maybe process `Related help topics' a la help xrefs and
    ;; allow C-c C-f in help buffer.
    (let* ((help-contents (buffer-substring (point-min) (point-max)))
	   (temp-buffer-show-hook	; avoid xref stuff
	    (lambda ()
	      (toggle-read-only 1)
	      (setq view-return-to-alist
		    (list (cons (selected-window) help-return-method))))))
      (with-output-to-temp-buffer (help-buffer)
	(with-current-buffer standard-output
	  ;; Fixme: Is this actually useful?
	(help-setup-xref (list 'python-describe-symbol symbol) (interactive-p))
	(set (make-local-variable 'comint-redirect-subvert-readonly) t)
	;; (print-help-return-message)
	;; Finally, display help contents
	(princ help-contents)
	)))
    ))

;;; Use icicles for completing-read if possible
(require 'icicles nil t)
