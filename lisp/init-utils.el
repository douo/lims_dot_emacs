;;; init-vars.el --- 定义全局通用变量.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; 根据操作系统执行代码
(defmacro with-system (type &rest body)
  "Evaluate BODY if `system-type' equals TYPE."
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Indenting-Macros.html
  (declare (indent defun))
  `(when (eq system-type ',type)
     ,@body))


(provide 'init-utils)

;;; init-utils.el ends here
