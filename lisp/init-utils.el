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


;; 通过 key description 查找命令
;; 支持正则表达式，比如 "^s-" 查找所有以 "s-" 开头的命令
(defun filter-commands-by-key ()
  "Filter Emacs commands based on the given key description string."
  (interactive)
  (let ((kd (read-regexp "Enter regular expression to match key description: ")))
    (mapatoms
     (lambda (symbol)
       (when (commandp symbol)
         (let ((key-bindings (where-is-internal symbol)))
           (when key-bindings
             (let* ((keys (mapcar 'key-description key-bindings))
                    (contains-kd (catch 'found (dolist (key keys)
                                                 (when (string-match kd key)
                                                   (throw 'found t))))))
               (when contains-kd
                 (message "Commands bound to %s: %s" keys symbol))))))))))

(provide 'init-utils)

;;; init-utils.el ends here
