;;;; `Pyrex' mode.

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.pyx\\'" . pyrex-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.pxi\\'" . pyrex-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.pxd\\'" . pyrex-mode))

;;;###autoload
(define-derived-mode pyrex-mode python-mode "Pyrex"
  (font-lock-add-keywords
   nil
   `((,(concat "\\<\\(NULL"
	       "\\|c\\(pdef\\|def\\|har\\|typedef\\|import\\)"
	       "\\|e\\(num\\|xtern\\)"
	       "\\|float"
	       "\\|in\\(clude\\|t\\)"
	       "\\|object\\|public\\|struct\\|type\\|union\\|void"
	       "\\)\\>")
      1 font-lock-keyword-face t))))

(provide 'pyrex)