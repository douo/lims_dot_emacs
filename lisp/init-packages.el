(print "package")
;;  __        __             __   ___
;; |__)  /\  /  ` |__/  /\  / _` |__
;; |    /~~\ \__, |  \ /~~\ \__> |___
;;                      __   ___        ___      ___
;; |\/|  /\  |\ |  /\  / _` |__   |\/| |__  |\ |  |
;; |  | /~~\ | \| /~~\ \__> |___  |  | |___ | \|  |
(when (>= emacs-major-version 24)
    (require 'package)
    (package-initialize)
    (setq package-archives '(("gnu"   . "https://mirrors.163.com/elpa/gnu/")
			 ("melpa" . "https://mirrors.163.com/elpa/melpa/"))))

;; cl - Common Lisp Extension
(require 'cl)

;; Add Packages
(defvar my/packages '(
			   ;; --- Auto-completion ---
			   company
			   ;; --- Better Editor ---
			   smooth-scrolling
			   hungry-delete
			   swiper
			   counsel
			   smartparens
			   popwin
			   ;; --- Major Mode ---
			   js2-mode
			   ruby-mode
			   markdown-mode
			   ;; --- Minor Mode ---
			   nodejs-repl
			   exec-path-from-shell
			   ) "Default package")
  


;; 文件末尾
(provide 'init-packages)
