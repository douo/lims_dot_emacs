;;  __        __             __   ___
;; |__)  /\  /  ` |__/  /\  / _` |__
;; |    /~~\ \__, |  \ /~~\ \__> |___
;;                      __   ___        ___      ___
;; |\/|  /\  |\ |  /\  / _` |__   |\/| |__  |\ |  |
;; |  | /~~\ | \| /~~\ \__> |___  |  | |___ | \|  |
(when (>= emacs-major-version 24)
    (require 'package)
    (package-initialize)
    (setq package-archives
	  ;; TNUA ELPA
	  ;; '(("gnu"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
          ;;   ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
	  ;; Emacs China ELPA
	  ;; '(("gnu"   . "https://elpa.emacs-china.org/gnu/")
          ;; ("melpa" . "https://elpa.emacs-china.org/melpa/")))
	  ;; 163
	  '(("gnu"   . "http://mirrors.163.com/elpa/gnu/")
            ("melpa" . "http://mirrors.163.com/elpa/melpa/")))
    )

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
			   neotree
			   ;; --- Major Mode ---
			   js2-mode
			   ruby-mode
			   kotlin-mode
			   markdown-mode
			   ;; --- Minor Mode ---
			   nodejs-repl
			   exec-path-from-shell
			   ) "Default package")
  

(setq package-selected-packages my/packages)

(defun my/packages-installed-p ()
     (loop for pkg in my/packages
	   when (not (package-installed-p pkg)) do (return nil)
	   finally (return t)))

 (unless (my/packages-installed-p)
     (message "%s" "Refreshing package database...")
     (package-refresh-contents)
     (dolist (pkg my/packages)
       (when (not (package-installed-p pkg))
	 (package-install pkg))))


;; 文件末尾
(provide 'init-packages)
