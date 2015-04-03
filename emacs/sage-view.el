;;; sage-view.el --- Typeset Sage output on the fly

;; Copyright (C) 2008  Matthias Meulien, Nick Alexander

;; Authors: Matthias Meulien <matthias.meulien@xlim.fr>, Nick Alexander
;; <ncalexander@gmail.com>
;; Keywords: sage math image

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `sage-view' typesets output in an `inferior-sage-mode' buffer and displays
;; plots inline in an `inferior-sage-mode'.  Inline displays are context
;; sensitive; by default, right-clicking brings up a context-menu.

;; Use `sage-view' to enable the minor mode, and then
;; `sage-view-enable-inline-output', `sage-view-disable-inline-output' and
;; `sage-view-enable-inline-plots', `sage-view-disable-inline-plots' enable
;; and disable the relevant features.  You might add some of those functions
;; to `sage-view-hook' to configure `sage-view' to your liking.

;; You can customize `sage-view' using the Emacs customize interface by M-x
;; customize-group RET sage-view RET.  In particular you can customize
;; magnification and margins

;; This mode was inspired by doc-view.el by Tassilo Horn, preview.el
;; by David Kastrup, and imath.el by Yasuaki Honda.

;; The LaTeX style used by preview.el is mandatory to use
;; sage-view.el.  It is shipped with AUCTeX.

;;; Todo:
;; - Add a auto-reveal stuff to overlays
;; - Check that display is image capable
;; - Disabling sage-view mode should remove overlays
;; - Set color, center image, enlarge overlay to window full size
;; - Add zoom features to overlays
;; - Add horizontal scrolling

;; Bugs:
;; - Numpy output can be a text array... should not be inserted into
;;   $$ signs (hum... example?)

;;; Code:
(require 'sage)
(require 'sage-mode)
(require 'mouse) ;; for popup-menu

(defcustom sage-view-latex-head
  "\\documentclass{article}
\\usepackage[active, tightpage, pdftex, displaymath]{preview}
\\usepackage{amstext}
\\begin{document}
\\begin{preview}
\\begin{math}\n"
  "String to be inserted at the top of a LaTeX document."
  :type 'string
  :group 'sage-view)

(defcustom sage-view-latex-tail
  "\n\\end{math}
\\end{preview}
\\end{document}\n"
  "String to be inserted at the end of a LaTeX document."
  :type 'string
  :group 'sage-view)

(defcustom sage-view-default-commands t
  "Determine what to enable when `sage-view' is started.
If equal to the symbol `plots' then will start inline plotting.
If equal to the symbol `output' then will start typesetting output.
Otherwise, if non-nil will start both.

Each of these can be enabled or disabled later by calling
`sage-view-enable-inline-plots', `sage-view-disable-inline-plots',
`sage-view-enable-inline-output', or `sage-view-disable-inline-output'."
  :type '(choice (const :tag "Inline Plots" plots)
		 (const :tag "Typeset Output" output)
		 (const :tag "Both" t))
  :group 'sage-view)

(defvar sage-view-inline-plots-enabled nil)
(defvar sage-view-inline-output-enabled nil)
(defcustom sage-view-inline-plots-method 'embedded-mode
  "Determine how plots are included inline.

This has an effect when enabling and disabling inline plots and
changing it's value before disabling will disable the new method,
leaving the old method in place.

If equal to the symbol `emacsclient' then it will use
emacsclient (as specified by `sage-view-emacsclient') to open the
file.  This should be robust across Sage versions, but requires
`sage-view-emacsclient' to be set correctly in order to work.

If equal to the symbol `emacsclient-window' then it will use
emacsclient as above, but open in a different window rather than
inline with the rest of the output.

If equal to the symbol `doctest-mode' then it will use
DOCTEST_MODE.  This is known to work on older versions of Sage,
but has problems in recent versions.

If equal to the symbol `embedded-mode' then it will use
EMBEDDED_MODE.  This is known to cause problems with some Sage
functionality like trace, since Sage thinks it's running in a
browser."
  :type '(choice (const :tag "Emacs client inline" emacsclient)
		 (const :tag "Emacs client in separate window" emacsclient-window)
		 (const :tag "Doctest mode" doctest-mode)
		 (const :tag "Embedded mode" embedded-mode))
  :set (lambda (symbol value)
	 (if (not (buffer-live-p sage-buffer))
	     (set symbol value)
	   (with-current-buffer sage-buffer
	     (let ((enabled sage-view-inline-plots-enabled))
	       (when enabled
		 (sage-view-disable-inline-plots))
	       (set symbol value)
	       (when enabled
		 (sage-view-enable-inline-plots))))))
  :group 'sage-view)

(defcustom sage-view-emacsclient
  ;; Magit goes to quite some length to find the right executable, so
  ;; use that if we have it.
  (or (and (fboundp 'magit-locate-emacsclient)
	   (magit-locate-emacsclient))
      (and invocation-directory
	   (file-executable-p (concat invocation-directory "emacsclient"))
	   (concat invocation-directory "emacsclient"))
      (and invocation-directory
	   (file-executable-p (concat invocation-directory "bin/emacsclient"))
	   (concat invocation-directory "bin/emacsclient"))
      (executable-find "emacsclient"))
  "The emacslient executable to use for viewing plots inline.

If `sage-view-inline-plots-method' is set to `emacsclient' or
`emacsclient-window', then this is used.  If this is set
incorrectly, then it's known to cause issues, at least on OS X."
  :type 'string
  :group 'sage-view)

(defcustom sage-view-emacsclient-wrapper
  (concat (file-name-directory
	   (or load-file-name
	       (buffer-file-name))) "sage-view.sh")
  "A wrapper script around emacsclient for when `sage-view-inline-plots-method' is emacsclient.

The script calls the function `sage-view-handle-emacslient'
instead of simply opening the image which is what happens when
`sage-view-inline-plots-method' is emacsclient.

In most cases you should not have to change this."
  :type 'string
  :group 'sage-view)

(defvar sage-view-start-string "<html><\\(?:span class=\"math\"\\|script type=\"math/tex\"\\)>"
  "HTML tags that identify the begining of a math formula in Sage output.")

(defvar sage-view-final-string "</\\(?:span\\|script\\)></html>"
  "HTML tags that identify the end of a math formula in Sage output.")

(defvar sage-view-plot-regex
  "<html>\\(?:<font color='[^']*'>\\)?<img src='cell://\\(sage[0-9]+.png\\)'>\\(?:</font>\\)?</html>"
  "HTML tags that identify a plot in Sage output.")

(defvar sage-view-output-regexp
  "BEGIN_TEXT:\\(\\(?:.\\|\n\\)*?\\):END_TEXT\nBEGIN_LATEX:\\(\\(?:.\\|\n\\)*?\\):END_LATEX"
  "Regular expression matching typeset output from BackendEmacs.")

(defvar sage-view-dir-name nil)

(defun sage-view-latex->pdf (ov)
  "Start conversion of the LATEX document associated to OV to PDF.

See also `sage-view-process-overlay'."
  (let* ((latex (concat (overlay-get ov 'file-sans-extension) ".tex"))
	 (options (append
		   (list (concat "--output-directory=" sage-view-dir-name)
			 (concat "-interaction=" "nonstopmode")
			 (concat "-output-format=" "pdf")
			 latex)))
	 (proc (apply 'start-process
		      (append (list "latex->pdf" nil "latex") options))))
    (process-put proc 'overlay ov)
    (set-process-sentinel proc 'sage-view-latex->pdf-sentinel)))

(defun sage-view-pdf->png (ov)
  "Start conversion of the PDF file associated to OV to PNG.

See also `sage-view-process-overlay'."
  (let* ((base (overlay-get ov 'file-sans-extension))
	 (png (concat base ".png"))
	 (pdf (concat base ".pdf"))
	 (level (int-to-string sage-view-anti-aliasing-level))
	 (scale (or (overlay-get ov 'scale) sage-view-scale))
	 (options (append
		   sage-view-gs-options
		   (list (concat "-dTextAlphaBits=" level)
			 (concat "-dGraphicsAlphaBits=" level)
			 (concat "-sOutputFile=" png)
			 (concat "-r" (sage-view-compute-resolution scale))
			 pdf)))
	 (proc (apply 'start-process
		      (append
		       (list "pdf->png" "*sage-view*" sage-view-gs-command)
		       options))))
    (process-put proc 'overlay ov)
    (set-process-sentinel proc 'sage-view-pdf->png-sentinel)))

(defvar sage-view-conversion-failed-map
  (let ((map (make-sparse-keymap))
	(l (lambda () (interactive)
	     (dolist (ov (overlays-at (point)))
	       (if (overlay-get ov 'file-sans-extension)
		   (find-file (concat (overlay-get ov 'file-sans-extension) ".log")))))))
    (define-key map "RET" l)
    (define-key map [return] l)
    (define-key map [mouse-1]
      (lambda (event) (interactive "e")
	(dolist (ov (overlays-at (posn-point (event-end event))))
	  (if (overlay-get ov 'file-sans-extension)
	      (find-file (concat (overlay-get ov 'file-sans-extension) ".log"))))))
    (define-key map [mouse-3]
      (lambda (event) (interactive "e")
	(dolist (ov (overlays-at (posn-point (event-end event))))
	  (if (overlay-get ov 'file-sans-extension)
	      (sage-view-context-menu ov event)))))
    map)
  "Keymap for overlays in which the conversion has failed.")

(defun sage-view-latex->pdf-sentinel (proc event)
  "If PROC (supposed to be a conversion process from LATEX to
PDF) was successful, convert the PDF to PNG.

See also `sage-view-process-overlay'."
  (let* ((ov (process-get proc 'overlay))
	 (base (overlay-get ov 'file-sans-extension)))
    (if (string-match "finished" event)
	(sage-view-pdf->png ov)
      (overlay-put ov 'keymap sage-view-conversion-failed-map)
      (overlay-put ov 'display
		   (concat "Conversion failed (see " base ".log" ")")))))

(defun sage-view-pdf->png-sentinel (proc event)
  "If PROC (supposed to be a conversion process from PDF to PNG)
was successful, update the overlay associated to PROC.

See also `sage-view-process-overlay'."
  (let* ((ov (process-get proc 'overlay))
	 (base (overlay-get ov 'file-sans-extension))
	 (png (concat base ".png"))
	 (image (when (and (string-match "finished" event)
			   png (file-readable-p png))
		  (append (list 'image :type 'png :file png :margin sage-view-margin)))))
    (if image
	(overlay-put ov 'display image)
      (overlay-put ov 'keymap sage-view-conversion-failed-map)
      (overlay-put ov 'display
		   (concat "Conversion failed (see " base ".log" ")")))
    (sit-for 0)))

(defun sage-view-compute-resolution (scale)
  (if (display-graphic-p)
      ;; In a terminal, display-mm-width returns nil and
      ;; display-pixel-width returns the number of characters.
      (let ((w (* scale (/ (* 25.4 (display-pixel-width))
			   (display-mm-width))))
	    (h (* scale (/ (* 25.4 (display-pixel-height))
			   (display-mm-height)))))
	(concat (int-to-string w) "x" (int-to-string h)))
    "72x72"))

(defun sage-view-process-overlay (ov)
  "Associate a LATEX document to OV and start conversion process
from LATEX to PDF.

The conversion process is done by `sage-view-latex->pdf'. When it
ends, `sage-view-latex->pdf-sentinel' is called: If the
conversion is successful, a conversion process from PDF to PNG
starts. When it ends, `sage-view-pdf->png-sentinel' is called: If
the last conversion is successful, OV displays the resulting
image."
  (let* ((base (expand-file-name
		(make-temp-name "sage-view_") sage-view-dir-name))
	 (file (concat base ".tex")))
    (with-temp-file file
      (insert sage-view-latex-head)
      (insert (overlay-get ov 'math))
      (insert sage-view-latex-tail)
      ;; The LaTeX created by Sage for MathJax (in some cases) isn't valid.
      ;; This is our attempt to work around it.
      (goto-char (point-min))
      (while (search-forward-regexp "\\verb!\\([^!]*\\)!"  nil t)
	(replace-match "\mathtt{\\1}")))
    (overlay-put ov 'file-sans-extension base)
    (sage-view-latex->pdf ov)))

(defun sage-view-output-filter-process-inline-output (string)
  "Substitute overlays to inline output.

Each region delimited by `sage-view-start-string' and
`sage-view-final-string' is replaced by an overlay.

This function expects the buffer to be narrowed to the current
output. And should be wrapped in a `save-excursion' and
`save-restriction' call.

See also `sage-view-output-filter'."
  (goto-char (point-min))
  (while (re-search-forward sage-view-start-string (point-max) t)
    (let* ((header-beg (match-beginning 0))
           (header-end (match-end 0))

           (footer-beg (when (re-search-forward sage-view-final-string (point-max) t)
                         (match-beginning 0)))
           (footer-end (match-end 0))
           (text (buffer-substring-no-properties header-end footer-beg))
           (ov (make-overlay header-beg
                             footer-end
                             nil nil nil))
           (map (make-sparse-keymap)))
      (overlay-put ov 'help-echo "mouse-3: Open contextual menu")
      (overlay-put ov 'math text)
      (overlay-put ov 'display (format "Typesetting %s..." text))
      (define-key map [mouse-3]
        `(lambda (event) (interactive "e")
           (sage-view-context-menu ,ov event)))
      (overlay-put ov 'keymap map)
      (sage-view-process-overlay ov)))
  ;; Sage 6.6 and on
  (goto-char (point-min))
  (while (re-search-forward sage-view-output-regexp (point-max) t)
    (let* ((full-beg (match-beginning 0))
           (full-end (match-end 0))
	   (text-beg (match-beginning 1))
           (text-end (match-end 1))
	   (latex-beg (match-beginning 2))
           (latex-end (match-end 2))
	   (text (buffer-substring-no-properties text-beg text-end))
	   (latex (buffer-substring-no-properties latex-beg latex-end))
           (ov (make-overlay full-beg full-end
			     nil nil nil))
	   (map (make-sparse-keymap)))
      ;; Delete everything except the text
      (delete-region text-end full-end)
      (delete-region full-beg text-beg)
      ;; Populate the overlay
      (overlay-put ov 'help-echo "mouse-3: Open contextual menu")
      (overlay-put ov 'text text)
      (overlay-put ov 'math latex)
      (define-key map [mouse-3]
        `(lambda (event) (interactive "e")
           (sage-view-context-menu ,ov event)))
      (overlay-put ov 'keymap map)
      (sage-view-process-overlay ov))))

(defun sage-view-output-filter-process-inline-plots (string)
  "Generate and place one overlay image for one inline plot,
found by looking for a particular png file in directory
`sage-view-dir-name'.

This function expects the buffer to be narrowed to just the
current output; see `sage-view-output-filter' for how to do
that."
  (goto-char (point-min))
  (while (re-search-forward sage-view-plot-regex (point-max) t)
    (let* ((html-beg (match-beginning 0))
           (html-end (match-end 0))
	   (pngname (match-string-no-properties 1))
	   (base (expand-file-name (make-temp-name "sage-view-plot_") sage-view-dir-name))
	   (pngname2 (concat base ".png")))
      (when (and pngname
		 (file-exists-p pngname)
		 (file-readable-p pngname))
	;; the found branch
	(rename-file pngname pngname2 t)
	(goto-char comint-last-input-end)
	(let ((im (create-image pngname2 'png))
	      (ov (make-overlay html-beg
				html-end
				nil nil nil))
	      (map (make-sparse-keymap)))
	  (overlay-put ov 'display im)
	  ;; help alignment as much as possible
	  (overlay-put ov 'before-string "\n")
	  (define-key map [mouse-3]
	    `(lambda (event) (interactive "e")
	       (sage-view-plot-context-menu ,ov event)))
	  (overlay-put ov 'keymap map))))))

(defun sage-view-output-filter (string)
  "Generate and place overlay images for inline output and inline plots.

Function to be inserted in `comint-output-filter-functions'."
  (save-excursion
    (save-restriction
      (narrow-to-region comint-last-input-end
			(process-mark (get-buffer-process (current-buffer))))
      (when sage-view-inline-plots-enabled
	(sage-view-output-filter-process-inline-plots string))
      (when (and sage-view-inline-output-enabled
		 (or (string-match sage-view-final-string string)
		     (string-match ":END_LATEX" string)))
	(sage-view-output-filter-process-inline-output string)))))

;;;###autoload
(defun sage-view-enable-inline-output ()
  "Enable inline output pretty-printing, i.e. typeset output from sage in the `inferior-sage-mode' buffer.
WARNING: this communicates with the sage process.  Only use this
when `sage-view' mode is enabled and sage is running."
  (interactive)
  ;; older sage
  (sage-send-command "if hasattr(sys.displayhook, 'set_display'): pretty_print_default(True)" nil t)
  ;; sage 5.12
  (sage-send-command "import IPython.core.ipapi; IPython.core.ipapi.get().magic('display typeset')" nil t)
  ;; Sage 6.6
  (sage-send-command "from sage.repl.rich_output.backend_emacs import BackendEmacs" nil t)
  (sage-send-command "from sage.repl.rich_output import get_display_manager" nil t)
  (sage-send-command "get_display_manager().switch_backend(BackendEmacs(),shell=get_ipython())" nil t)
  ;; fallthrough
  ;; Sage 6.2
  (sage-send-command "get_ipython().magic('display typeset')" nil t)
  (setq sage-view-inline-output-enabled t)
  (sage-view-update-modeline))

;;;###autoload
(defun sage-view-disable-inline-output ()
  "Disable inline output pretty-printing, i.e. do not typeset output from sage in the `inferior-sage-mode' buffer.
WARNING: this communicates with the sage process.  Only use this
when `sage-view' mode is enabled and sage is running."
  (interactive)
  ;; older sage
  (sage-send-command "if hasattr(sys.displayhook, 'set_display'): pretty_print_default(False)" nil t)
  ;; sage 5.12
  (sage-send-command "import IPython.core.ipapi; IPython.core.ipapi.get().magic('display')" nil t)
  ;; Sage 6.2
  (sage-send-command "get_ipython().magic('display simple')" nil t)
  (setq sage-view-inline-output-enabled nil)
  (sage-view-update-modeline))

;;;###autoload
(defun sage-view-enable-inline-plots ()
  "Enable inline plotting, i.e. display plots in the `inferior-sage-mode' buffer and do not spawn an external viewer.
WARNING: this communicates with the sage process.  Only use this
when `sage-view' mode is enabled and sage is running."
  (interactive)
  (cond
   ((or (eq sage-view-inline-plots-method 'emacsclient)
	(eq sage-view-inline-plots-method 'emacsclient-window))
    (when (not (server-running-p))
      (error "Server appears to not be running.  Try adding (server-start) to your .emacs"))
    ;; Store the old viewer
    (sage-send-command "_emacs_sage_viewer_ = _emacs_sage_viewer_ if '_emacs_sage_viewer_' in globals() else sage.misc.viewer.viewer.png_viewer()" nil t)
    (sage-send-command (concat "sage.misc.viewer.viewer.png_viewer('''"
			       (when (eq sage-view-inline-plots-method 'emacsclient)
				 sage-view-emacsclient-wrapper)
			       " "
			       sage-view-emacsclient
			       "''')")
		       nil t))
   ((eq sage-view-inline-plots-method 'embedded-mode)
    (sage-send-command "sage.plot.plot.EMBEDDED_MODE = True;" nil t))
   ((eq sage-view-inline-plots-method 'doctest-mode)
    (sage-send-command "sage.plot.plot.DOCTEST_MODE = True;" nil t)
    ;; sage 4.7
    (sage-send-command
     (format "sage.plot.plot.DOCTEST_MODE_FILE = '%s/sage-view.png';"
	     sage-view-dir-name)
     nil t)
    ;; sage 5.0
    (sage-send-command
     (format "if hasattr(sage.plot,'graphics'): sage.plot.graphics.DOCTEST_MODE_FILE = '%s/sage-view.png';\n"
	     sage-view-dir-name)
     nil t)
    ;; sage 5.something
    (sage-send-command "if hasattr(sage,'doctest'): sage.doctest.DOCTEST_MODE = True;\n" nil t))
   (t
    (error "Unknown value for `sage-view-inline-plots-method'")))
  (setq sage-view-inline-plots-enabled t)
  (sage-view-update-modeline))

;;;###autoload
(defun sage-view-disable-inline-plots ()
  "Disable inline plotting, i.e. do not display plots in the `inferior-sage-mode' buffer and instead spawn an external viewer.
WARNING: this communicates with the sage process.  Only use this
when `sage-view' mode is enabled and sage is running."
  (interactive)
  (cond
   ((or (eq sage-view-inline-plots-method 'emacsclient)
	(eq sage-view-inline-plots-method 'emacsclient-window))
    (sage-send-command (concat "sage.misc.viewer.viewer.png_viewer(_emacs_sage_viewer_)")
		       nil t))
   ((eq sage-view-inline-plots-method 'embedded-mode)
    (sage-send-command "sage.plot.plot.EMBEDDED_MODE = False;" nil t))
   ((eq sage-view-inline-plots-method 'doctest-mode)
    (sage-send-command "sage.plot.plot.DOCTEST_MODE = False;" nil t)
    ;; sage 4.7
    (sage-send-command "sage.plot.plot.DOCTEST_MODE_FILE = None;" nil t)
    ;; sage 5.0
    (sage-send-command "if hasattr(sage.plot,'graphics'): sage.plot.graphics.DOCTEST_MODE_FILE = None;\n" nil t)
    ;; sage 5.something
    (sage-send-command "if hasattr(sage,'doctest'): sage.doctest.DOCTEST_MODE = False;\n" nil t))
   (t
    (error "Unknown value for `sage-view-inline-plots-method'")))
  (setq sage-view-inline-plots-enabled nil)
  (sage-view-update-modeline))

(defun sage-view-handle-emacslient (image-file)
  (when (buffer-live-p sage-buffer)
    (with-current-buffer sage-buffer
      (save-excursion
	(goto-char comint-last-output-start)
	;; Go to a blank line (where it should be output, but if not don't worry)
	(re-search-backward "^$" comint-last-input-end t)
	(let* ((inhibit-read-only t)
	       (im (create-image image-file 'png))
	       (p (point))
	       (junk (insert (concat "image:" image-file)))
	       (ov (make-overlay p (point) nil nil nil))
	       (map (make-sparse-keymap)))
	  (overlay-put ov 'display im)
	  ;; help alignment as much as possible
	  (overlay-put ov 'before-string "\n")
	  (overlay-put ov 'after-string "\n")
	  (define-key map [mouse-3]
	    `(lambda (event) (interactive "e")
	       (sage-view-plot-context-menu ,ov event)))
	  (overlay-put ov 'keymap map))))))

(defun sage-view-create-temp ()
  "Create a temporary directory and set `sage-view-dir-name'
to its name.

Nothing is done if `sage-view-dir-name' equals the name of a
writable directory."
  (unless (and sage-view-dir-name
	       (file-directory-p sage-view-dir-name)
	       (file-writable-p sage-view-dir-name))
    ;; I'm not sure SAGE_TESTDIR is the right thing, but it's
    ;; .sage/tmp which seems like what I should use.
    (let ((tmp-dir (shell-command-to-string (concat sage-command " -sh -c 'echo -n $SAGE_TESTDIR' 2>/dev/null"))))
      (when (= 0 (length tmp-dir))
	(setq tmp-dir "~/.sage/tmp"))
      (setq sage-view-dir-name
	    (make-temp-name (expand-file-name "emacs" tmp-dir))))
    (condition-case err
	(make-directory sage-view-dir-name t)
      (error (message "Creation of `%s' failed: %s"
		      sage-view-dir-name (error-message-string err))))))

(defun sage-view-delete-temp ()
  "Delete the directory named after `sage-view-dir-name'."
  (when sage-view-dir-name
    (condition-case err
	(progn
	  (mapc #'delete-file
		(directory-files sage-view-dir-name t "^sage-view" t))
	  (delete-directory sage-view-dir-name))
      (error (message "Deletion of `%s' failed: %s"
		      sage-view-dir-name (error-message-string err))))))

(defun sage-view-overlay-activep (ov)
  "Check whether there is a valid image associated with OV."
  (eq (car (overlay-get ov 'display)) 'image))

(defun sage-view-cleanup-copied-text (str)
  "Remove some boilerplate text added by Sage to all LaTeX output."
  (replace-regexp-in-string
   (regexp-quote "\\newcommand{\\Bold}[1]{\\mathbf{#1}}")
   "" str))

(defun sage-view-copy-text (ov)
  "Copy text source of OV into the kill buffer."
  (let ((text (overlay-get ov 'text)))
    (if text
	(kill-new text)
      (message "No text available"))))

(defun sage-view-copy-latex (ov)
  "Copy LaTeX source of OV into the kill buffer."
  (let ((text (overlay-get ov 'math)))
    (if text
	(kill-new (sage-view-cleanup-copied-text text))
      (message "No LaTeX code available"))))

(defun sage-view-save-image (ov)
  "Copy image file associated to OV.

Make sure that there is a valid image associated with OV with
`sage-view-overlay-activep'."
  (let* ((spec (cdr (overlay-get ov 'display)))
	 (file (plist-get spec :file))
	 (name (when (and file (file-readable-p file))
		 (expand-file-name
		  (read-file-name "Write image to file: "
				 default-directory
				 "sage-view.png")))))
    (if name
	(copy-file file name))))

(defun sage-view-regenerate (ov)
  "Return zoom to normal and regenerate the overlay."
  (overlay-put ov 'scale sage-view-scale)
  (sage-view-process-overlay ov))

(defun sage-view-zoom-in (ov &optional multiplier)
  "Internal function to zoom in on an overlay."
  (unless (numberp multiplier)
    (setq multiplier 1))
  (let ((scale (or (overlay-get ov 'scale) sage-view-scale)))
    (overlay-put ov 'scale (+ scale (* multiplier sage-view-scale-factor)))
    (message "Overlay's scale set to %s" scale)
    (sage-view-process-overlay ov)))

(defun sage-view-zoom-out (ov &optional multiplier)
  "Internal function to zoom out on an overlay."
  (unless (numberp multiplier)
    (setq multiplier 1))
  (let* ((scale (or (overlay-get ov 'scale) sage-view-scale))
	 (new-scale (- scale (* multiplier sage-view-scale-factor))))
    ;; Ensure it's not too small (or negative)
    (when (< new-scale sage-view-scale-factor)
      (setq new-scale sage-view-scale-factor))
    (overlay-put ov 'scale new-scale)
    (message "Overlay's scale set to %s" scale)
    (sage-view-process-overlay ov)))

(defun sage-view-context-menu (ov ev)
  "Pop up a menu for OV at position EV."
  (popup-menu
   `("Sage View Mode"
     ["Regenerate" (lambda () (interactive) (sage-view-regenerate ,ov))]
     ["Copy Text"  (lambda () (interactive) (sage-view-copy-text ,ov))]
     ["Copy LaTeX" (lambda () (interactive) (sage-view-copy-latex ,ov))]
     ["Save As..." (lambda () (interactive) (sage-view-save-image ,ov))
      `(sage-view-overlay-activep ,ov)]
     ["Zoom in" (lambda (multiplier) (interactive "p")
		  (sage-view-zoom-in ,ov multiplier))
      `(sage-view-overlay-activep ,ov)]
     ["Zoom out" (lambda (multiplier) (interactive "p")
		   (sage-view-zoom-out ,ov multiplier))
      `(sage-view-overlay-activep ,ov)]
     "--"
     ["Customize Conversion Options" (lambda () (interactive)
				       (customize-group 'sage-view t))
      `(sage-view-overlay-activep ,ov)])
   ev))

(defun sage-view-plot-context-menu (ov ev)
  "Pop up a menu for OV at position EV."
  (popup-menu
   `("Sage View Mode"
     ["Save As..." (lambda () (interactive) (sage-view-save-image ,ov))
      `(sage-view-overlay-activep ,ov)])
   ev))

;; Inspired by c-update-modeline
(defun sage-view-update-modeline ()
  "Update modeline to include information about whether sage-view is enabled."
  (when (eq major-mode 'inferior-sage-mode)
    (let ((fmt (format "/%s%s"
		       (if sage-view-inline-plots-enabled "p" "")
		       (if sage-view-inline-output-enabled "t" "")))
	  (bare-mode-name (if (string-match "\\(^[^/]*\\)/" mode-name)
			      (match-string 1 mode-name)
			    mode-name)))
      (setq mode-name
	    (if (> (length fmt) 1)
		(concat bare-mode-name fmt)
	      bare-mode-name))
      (force-mode-line-update))))

;; TODO: require a graphics console before turning it on, or make it possile to turn it on
;;;###autoload
(define-minor-mode sage-view
  "Toggle automatic typesetting of Sage output.

Typesetting of math formulas is done by LATEX subprocesses and
PDF to PNG conversions." nil
  :group 'sage-view
  :lighter " Sage-View"
  (if sage-view
      (progn
	(make-local-variable 'sage-view-dir-name)
	(sage-view-create-temp)
	(cond
	 ((eq sage-view-default-commands 'plots)
	  (sage-view-enable-inline-plots))
	 ((eq sage-view-default-commands 'output)
	  (sage-view-enable-inline-output))
	 (sage-view-default-commands
	  (sage-view-enable-inline-plots)
	  (sage-view-enable-inline-output)))
	(make-local-variable 'comint-output-filter-functions)
	(add-hook 'comint-output-filter-functions 'sage-view-output-filter)
	(add-hook 'kill-buffer-hook 'sage-view-delete-temp))
    (progn
      (sage-view-disable-inline-output)
      (sage-view-disable-inline-plots)
      (remove-hook 'comint-output-filter-functions 'sage-view-output-filter))))

(provide 'sage-view)
;;; sage-view.el ends here
