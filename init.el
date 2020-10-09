;; 根据操作系统执行代码
(defmacro with-system (type &rest body)
  "Evaluate BODY if `system-type' equals TYPE."
  (declare (indent defun))
  `(when (eq system-type ',type)
     ,@body))
;; 设置 emacsformacosx option 作为 meta 键
(with-system darwin
   (setq mac-option-modifier   'meta))
;; 自动加载外部修改过的文件
(global-auto-revert-mode 1)
;; 禁止 Emacs 自动生成备份文件，例如 init.el~ 。
(setq make-backup-files nil)
;; the toolbar is just a waste of valuable screen estate
;; in a tty tool-bar-mode does not properly auto-load, and is
;; already disabled anyway
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

;; disable startup screen
(setq inhibit-startup-screen t)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Emacs modes typically provide a standard means to change the
;; indentation width -- eg. c-basic-offset: use that to adjust your
;; personal indentation width, while maintaining the style (and
;; meaning) of any files you load.
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 8)            ;; but maintain correct appearance

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;;初始化包管理器
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
;; update the package metadata is the local cache is missing
(unless package-archive-contents
  (package-refresh-contents))

;; 安装 use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t)

(use-package recentf
  :bind (("C-x C-r" . 'recentf-open-files))
  :config
  (setq recentf-max-menu-items 10
        recentf-max-saved-items 25)
  (recentf-mode +1))

;; 高亮当前行
(use-package hl-line
  :config
  (global-hl-line-mode +1))

;; 同文件名的 buffer 名更容易识别
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

;; saveplace 上次保存文件时光标的位置
(use-package saveplace
  :config
  ;; activate it for all buffers
  (save-place-mode +1))

;;; 高亮括号
(use-package paren
  :config
  (show-paren-mode +1))

;; 自动补全引号、括号等
(use-package elec-pair
  :config
  (electric-pair-mode +1))

(use-package dired
  :config
  ;; dired - reuse current buffer by pressing 'a'
  (put 'dired-find-alternate-file 'disabled nil)

  ;; always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)

  ;; if there is a dired buffer displayed in the next window, use its
  ;; current subdir, instead of the current subdir of this dired buffer
  (setq dired-dwim-target t)

  ;; enable some really cool extensions like C-x C-j(dired-jump)
  (require 'dired-x))

;;主题
(use-package material-theme
  :ensure t
  :config
  (load-theme 'material t))

;;光标移动方案
;; https://github.com/abo-abo/avy
(use-package avy
  :ensure t
  :bind (("C-." . avy-goto-word-or-subword-1)
         ("M-g c" . avy-goto-char)
         ("M-g f" . avy-goto-line)
         )
  :config
  (setq avy-background t))

;; git
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))
(use-package git-timemachine
  :ensure t
  :bind (("s-g" . git-timemachine)))

;; https://agel.readthedocs.io/en/latest/index.html
(use-package ag
  :ensure t)

;; https://github.com/magnars/expand-region.el
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;; 显示匹配数量
(use-package anzu
  :ensure t
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode))

(use-package easy-kill
  :ensure t
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill))

;;On OS X (and perhaps elsewhere) the $PATH environment variable and
;; `exec-path' used by a windowed Emacs instance will usually be the
;; system-wide default path, rather than that seen in a terminal window.
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

;; 移动整个选择文本
(use-package move-text
  :ensure t
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift down)] . move-text-down)))

;; 用不同颜色区别嵌套的括号引号等
(use-package rainbow-delimiters
  :ensure t)
(use-package rainbow-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode))

;; 空格可视化
(use-package whitespace
  :init
  (dolist (hook '(prog-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  ; (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style '(face tabs empty trailing lines-tail)))

;; temporarily highlight changes from yanking, etc
(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode +1))

;; 补全
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.5)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (setq company-tooltip-flip-when-above t)
  (progn
    ;; Use Company for completion
    (bind-key [remap completion-at-point] #'company-complete company-mode-map)

    (setq company-tooltip-align-annotations t
          ;; Easy navigation to candidates with M-<n>
          company-show-numbers t)
    (setq company-dabbrev-downcase nil))
  (global-company-mode))


(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package super-save
  :ensure t
  :config
  (super-save-mode +1))

(use-package crux
  :ensure t
  :bind (("C-c o" . crux-open-with)
         ("M-o" . crux-smart-open-line)
         ("C-c n" . crux-cleanup-buffer-or-region)
         ("C-c f" . crux-recentf-find-file)
         ("C-M-z" . crux-indent-defun)
         ("C-c u" . crux-view-url)
         ("C-c e" . crux-eval-and-replace)
         ("C-c w" . crux-swap-windows)
         ("C-c D" . crux-delete-file-and-buffer)
         ("C-c r" . crux-rename-buffer-and-file)
         ("C-c t" . crux-visit-term-buffer)
         ("C-c k" . crux-kill-other-buffers)
         ("C-c TAB" . crux-indent-rigidly-and-copy-to-clipboard)
         ("C-c I" . crux-find-user-init-file)
         ("C-c S" . crux-find-shell-init-file)
         ("s-r" . crux-recentf-find-file)
         ("s-j" . crux-top-join-line)
         ("C-^" . crux-top-join-line)
         ("s-k" . crux-kill-whole-line)
         ("C-<backspace>" . crux-kill-line-backwards)
         ("s-o" . crux-smart-open-line-above)
         ([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ([(shift return)] . crux-smart-open-line)
         ([(control shift return)] . crux-smart-open-line-above)
         ([remap kill-whole-line] . crux-kill-whole-line)
         ("C-c s" . crux-ispell-word-then-abbrev)))

(use-package undo-tree
  :ensure t
  :config
  ;; autosave the undo-tree history
  (setq undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t))

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume))

(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "s-w") 'ace-window)
  (global-set-key [remap other-window] 'ace-window))

(use-package swiper
  :ensure t
  :config
  (setq ivy-use-group-face-if-no-groups t)
  (global-set-key "\C-s" 'swiper))

(use-package counsel
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c a") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

;; temporarily highlight changes from yanking, etc
(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode +1))

;; 主模式

;; ruby
(use-package inf-ruby
  :ensure t
  :config
  (add-hook 'ruby-mode-hook #'inf-ruby-minor-mode))
(use-package ruby-mode
  :config
  (setq ruby-insert-encoding-magic-comment nil)
  (add-hook 'ruby-mode-hook #'subword-mode))

;; golang
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred))

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; Optional - provides fancier overlays.
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

;; 需安装 gocode goimports
(use-package go-mode
  :ensure t)


(use-package go-eldoc
  :ensure t
  :defer
  :init
  (add-hook 'go-mode-hook 'go-eldoc-setup))

;; Markdown
(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :config
  (setq markdown-fontify-code-blocks-natively t)
  :preface
  (defun jekyll-insert-image-url ()
    (interactive)
    (let* ((files (directory-files "../assets/images"))
           (selected-file (completing-read "Select image: " files nil t)))
      (insert (format "![%s](/assets/images/%s)" selected-file selected-file))))

  (defun jekyll-insert-post-url ()
    (interactive)
    (let* ((files (remove "." (mapcar #'file-name-sans-extension (directory-files "."))))
           (selected-file (completing-read "Select article: " files nil t)))
      (insert (format "{%% post_url %s %%}" selected-file)))))


(use-package lua-mode
  :ensure t
  :mode "\\.lua\\'")


(use-package adoc-mode
  :ensure t
  :mode "\\.adoc\\'")

(use-package yaml-mode
  :ensure t)

(use-package cask-mode
  :ensure t)

(use-package load-relative
  :ensure t)

(load-relative "lisp/uci-mode.el") ;; openwrt uci config file
(require 'uci-mode)


;; End
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (load-relative uci-mode cask-mode yaml-mode adoc-mode markdown-mode inf-ruby counsel swiper ace-window ivy undo-tree crux super-save flycheck company volatile-highlights rainbow-mode rainbow-delimiters move-text exec-path-from-shell easy-kill anzu expand-region ag git-timemachine magit avy material-theme use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
