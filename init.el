(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ido-everywhere nil)
 '(ido-mode (quote both) nil (ido))
 '(inhibit-startup-screen nil)
 '(initial-buffer-choice nil)
 '(make-backup-files nil)
 '(mouse-avoidance-mode (quote animate) nil (avoid))
 '(mouse-drag-copy-region t)
 '(mouse-yank-at-point t)
;; '(py-default-interpreter "python2")
;; '(py-python-command "python2")
)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
)
;; 
(defun d-add-load-path-dfs (base)
  ;; ����Ŀ¼ base �е� init.el
  (add-to-list 'load-path base)
  (let ((init_file (concat base "/init.el")))
  ;;   (when (file-exists-p init_file)
  (load init_file)
  ;;  )
  )

  (dolist (f (directory-files base))  ;;������ for(f : set)�Ľṹ
    (let ((name (concat base "/" f)))
      (when (and (file-directory-p name) ;;��name ��һ��Ŀ¼ʱ ����t
                 (not (equal f ".."))
                 (not (equal f ".")))
	;; �����Ŀ¼����init.el ���������
	(let ((init_file (concat name "/init.el")))
	  (when (file-exists-p init_file)
	    (d-add-load-path-dfs name)
	    )
	  )
	)
      )))

;; ��ʼɨ��Ŀ¼
(d-add-load-path-dfs "~/.emacs.d/extensions")


;; ����
(setq user-full-name "Tiou Lims")
(setq user-mail-address "dourokinga@gmail.com")