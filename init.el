(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("d6a00ef5e53adf9b6fe417d2b4404895f26210c52bb8716971be106550cea257" "238b07eba71dd5d4f17c80aead6ec703407e8c32d66de9d695638834a0288898" default)))
 '(ido-everywhere nil)
 '(ido-mode (quote both) nil (ido))
 '(inhibit-startup-screen nil)
 '(initial-buffer-choice nil)
 '(make-backup-files nil)
 '(mouse-avoidance-mode (quote animate) nil (avoid))
 '(mouse-drag-copy-region t)
 '(mouse-yank-at-point t)
 '(send-mail-function (quote smtpmail-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq coding-system-for-read 'utf-8-unix)
(setq coding-system-for-write 'utf-8-unix)

;; 
(defun d-add-load-path-dfs (base)
  ;; 加载目录 base 中的 init.el
  (add-to-list 'load-path base)
  (let ((init_file (concat base "/init.el")))
  ;;   (when (file-exists-p init_file)
  (load init_file)
  ;;  )
  )

  (dolist (f (directory-files base))  ;;类似于 for(f : set)的结构
    (let ((name (concat base "/" f)))
      (when (and (file-directory-p name) ;;当name 是一个目录时 返回t
                 (not (equal f ".."))
                 (not (equal f ".")))
	;; 如果子目录中有init.el 则继续加载
	(let ((init_file (concat name "/init.el")))
	  (when (file-exists-p init_file)
	    (d-add-load-path-dfs name)
	    )
	  )
	)
      )))

;; 开始扫描目录
(d-add-load-path-dfs "~/.emacs.d/extensions")


;; 其他
(setq user-full-name "Tiou Lims")
(setq user-mail-address "dourokinga@gmail.com")
