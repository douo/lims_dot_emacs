;;; init-vars.el --- 定义全局通用变量. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

(defvar douo/writing-home (getenv "WRITING_HOME"))
(provide 'init-vars)

;;; init-vars.el ends here
