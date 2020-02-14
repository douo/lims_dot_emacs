;;; uci-mode.el --- Major mode for UCI configuration files

;; Public Domain

;; Author: Juho Juopperi <jkj@kapsi.fi>
;; URL: http://github.com/jkjuopperi/uci-mode
;; Version: 1.0.0

;; Add dir to load-path and (require 'uci-mode)

(defvar uci-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for `uci-mode'.")

(defvar uci-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table for `uci-mode'.")

(defvar uci-font-lock-keywords
  '(("\\<\\(config\\|option\\|list\\)\\>" (1 font-lock-builtin-face)))
  "Keyword highlighting specification for `uci-mode'.")

;;;###autoload
(define-derived-mode uci-mode fundamental-mode "UCI"
  "A major mode for editing UCI configuration files."
  :syntax-table uci-mode-syntax-table
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-start-skip) "#+\\s-*")
  (set (make-local-variable 'font-lock-defaults)
       '(uci-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'uci-indent-line))

(defun uci-indent-line ()
  "Indent current line of UCI configuration."
  (interactive)
  (let ((savep (> (current-column) (current-indentation)))
	(indent (condition-case nil (max (uci-calculate-indentation) 0)
		  (error 0))))
    (if savep
	(save-excursion (indent-line-to indent))
      (indent-line-to indent))))

(defun uci-calculate-indentation ()
  "Return the column to which the current line should be indented."
  (save-excursion
    (beginning-of-line)
    (cond ((looking-at "^[ \t]*\\(option\\|list\\)") tab-width)
	  ((looking-at "^[ \t]*config") 0)
	  (t tab-width))))

(provide 'uci-mode)

;;; uci-mode.el ends here
