;;; init-vars.el --- 定义全局通用变量. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

(defvar douo/writing-home (or (getenv "WRITING_HOME")
                              (progn
                                (warn "WRITING_HOME is not set")
                                (getenv "HOME"))))
(provide 'init-vars)

;;; init-vars.el ends here
