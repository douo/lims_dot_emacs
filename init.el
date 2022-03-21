;;; init.el --- douo's emacs config
;;; Commentary:
;;
;;; 个人用

;; Move customization variables to sparate file
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)


;;
;;; Code:
;; 根据操作系统执行代码
(defmacro with-system (type &rest body)
  "Evaluate BODY if `system-type' equals TYPE."
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Indenting-Macros.html
  (declare (indent defun))
  `(when (eq system-type ',type)
     ,@body))
;; 设置 emacsformacosx option 作为 meta 键
(with-system darwin
  (setq mac-option-modifier   'meta))

;; windows
(with-system windows-nt
  (setq w32-pass-rwindow-to-system nil)
  (setq w32-rwindow-modifier 'super) ; Right Windows key
  (w32-register-hot-key [s-]))


;; 自动加载外部修改过的文件，如果当前 buffer 未修改
;; revert buffers automatically when underlying files are changed externally
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


(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;;初始化包管理器

;; https://emacs.stackexchange.com/questions/37904/how-do-i-work-out-what-the-problem-is-with-the-emacs-package-system/56067#56067
(custom-set-variables

 '(gnutls-algorithm-priority "normal:-vers-tls1.3"))
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
;;(setq package-archives
;; TNUA ELPA
;; '(("gnu"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
;;   ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
;; Emacs China ELPA
;; '(("gnu"   . "https://elpa.emacs-china.org/gnu/")
;; ("melpa" . "https://elpa.emacs-china.org/melpa/")))
;; 163
;;'(("gnu"   . "http://mirrors.163.com/elpa/gnu/")
;;  ("melpa" . "http://mirrors.163.com/elpa/melpa/")))
;; update the package metadata is the local cache is missing
(unless package-archive-contents
  (package-refresh-contents))

;; 安装 use-package
;;https://phenix3443.github.io/notebook/emacs/modes/use-package-manual.html
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

(setq use-package-verbose t)

(use-package load-relative
  :ensure t)
;; 加载本机特殊配置，环境变量等...
(load-relative "local.el")

;; Library for converting first letter of Pinyin to Simplified/Traditional Chinese characters.
(use-package pinyinlib
  :ensure t)


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

;;主题
(use-package material-theme
  :ensure t
  :config
  (load-theme 'material t))

;;光标移动方案
;; https://github.com/abo-abo/avy
(use-package avy
  :ensure t
  :bind
  ("M-g w" . avy-goto-word-or-subword-1)
  ("M-g c" . avy-goto-char)
  :config
  (setq avy-background t))
;; avy 支持拼音
(use-package ace-pinyin
  :ensure t
  :config
  ;;(setq ace-pinyin-treat-word-as-char nil)
  (setq ace-pinyin-simplified-chinese-only-p nil)
  (ace-pinyin-global-mode +1)
  )
;; git
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))
(use-package git-timemachine
  :ensure t
  :bind (("M-g t" . git-timemachine)))

;; rg
(use-package rg
  :ensure t
  :config
  (rg-enable-default-bindings))

;; https://github.com/magnars/expand-region.el
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))
;; 更强大的 kill&yank
;; 代替 expand-region?
(use-package easy-kill
  :ensure t
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill))

;; 显示匹配数量
(use-package anzu
  :ensure t
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode))

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
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  )

;; 代码中的颜色值可视化
(use-package rainbow-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode))

;; Virtual comments
;; 为文件增加注释，与文件保存分离
(use-package virtual-comment
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'virtual-comment-mode)
  )


;; 空格可视化
(use-package whitespace
  :init
  (dolist (hook '(prog-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-style '(face tabs empty trailing )))

;; temporarily highlight changes from yanking, etc
(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode +1))

;; Save Emacs buffers when they lose focus
(use-package super-save
  :ensure t
  :config
  (super-save-mode +1))

;; 旋转 frame 布局
(use-package transpose-frame
  :ensure t
  )


;; A Collection of Ridiculously Useful eXtensions for Emacs
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
         ))

(use-package undo-tree
  :ensure t
  :config
  ;; autosave the undo-tree history
  (setq undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t))


;; 核心扩展
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
  (global-company-mode))


(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "s-w") 'ace-window)
  (global-set-key [remap other-window] 'ace-window))


;; alternative to the built-in Emacs help that provides much more contextual information.
(use-package helpful
  :ensure t
  :bind
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)
  )

;; Enable vertico
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
;; 可以实现访问越频繁的项越靠前
(use-package savehist
  :ensure t
  :init
  (savehist-mode))

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  :after vertico
  :ensure t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))


;; Example configuration for Consult
(use-package consult
  :ensure t
  :preface
  ;; exclude Tramp buffers from preview
  (defun consult-buffer-state-no-tramp ()
    "Buffer state function that doesn't preview Tramp buffers."
    (let ((orig-state (consult--buffer-state))
          (filter (lambda (cand restore)
                    (if (or restore
                            (let ((buffer (get-buffer cand)))
                              (and buffer
                                   (not (file-remote-p (buffer-local-value 'default-directory buffer))))))
                        cand
                      nil))))
      (lambda (cand restore)
        (funcall orig-state (funcall filter cand restore) restore))))
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("C-s" . consult-line)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi))           ;; needed by consult-line to detect isearch

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI. You may want to also
  ;; enable `consult-preview-at-point-mode` in Embark Collect buffers.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; Optionally configure a function which returns the project root directory.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (project-roots)
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project)))))
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-root-function #'projectile-project-root)
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-root-function #'vc-root-dir)
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-root-function (lambda () (locate-dominating-file "." ".git")))
  ;; Do not preview EXWM windows or Tramp buffers
  :custom
  (consult--source-buffer
   (plist-put consult--source-buffer :state #'consult-buffer-state-no-tramp))
  )

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package orderless
  :ensure t
  :after (pinyinlib)
  :preface
  ;; https://emacs-china.org/t/vertico/17913/3
  (defun completion--regex-pinyin (str)
    (orderless-regexp (pinyinlib-build-regexp-string str)))
  :config
  (add-to-list 'orderless-matching-styles 'completion--regex-pinyin)
  :custom (completion-styles '(orderless)))
;; end vertico

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; temporarily highlight changes from yanking, etc
(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode +1))

(use-package flymake
  :ensure t)

(use-package eglot
  :ensure t
  :hook
  ((prog-mode . (lambda ()
                 (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode))
                 (eglot-ensure)
                 )))
  )


;; 主模式


;; python

;; 只有安装了 conda 才启用
(when (and
       (getenv "CONDA_EXE")
       (file-exists-p (getenv "CONDA_EXE")))
  (let ((home
        (file-name-directory (directory-file-name (file-name-directory (getenv "CONDA_EXE"))))))
    (use-package conda
      :ensure t
      :config
      (conda-env-initialize-interactive-shells)
      (conda-env-initialize-eshell)
      :custom
      (conda-anaconda-home (expand-file-name home))
      (conda-env-home-directory (expand-file-name home))
      )
    )
  )


;; typescript
;; web-mode
;; (setq web-mode-markup-indent-offset 2)
;; (setq web-mode-code-indent-offset 2)
;; (setq web-mode-css-indent-offset 2)
(use-package web-mode
  :ensure t
  :mode (("\\.js\\'" . web-mode)
	 ("\\.jsx\\'" .  web-mode)
	 ("\\.ts\\'" . web-mode)
	 ("\\.tsx\\'" . web-mode)
	 ("\\.html\\'" . web-mode))
  :commands web-mode)


;; ruby

(use-package ruby-mode
  :ensure t
  :custom
  (ruby-insert-encoding-magic-comment nil)
  :hook
  (ruby-mode . #'subword-mode))


;;provides a REPL buffer connected to a Ruby subprocess.
(use-package inf-ruby
  :ensure t
  :after ruby-mode
  :hook
  (ruby-mode . #'inf-ruby-minor-mode))


;; golang
;; 需安装 goimports gopls
(use-package go-mode
  :ensure t)
;; end golang

;; Markdown
(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :custom
  (markdown-fontify-code-blocks-natively t)
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


(use-package yaml-mode
  :ensure t)

(use-package cask-mode
  :ensure t)

(use-package nginx-mode
  :ensure t
  )


(load-relative "lisp/uci-mode.el") ;; openwrt uci config file
(require 'uci-mode)


(load-relative "org.el")

(use-package sis
  :ensure t
  :config
  ;; macos
  ;; 使用系统输入法
  ;; 需要先安装 https://github.com/laishulu/macism
  (with-system darwin
    (sis-ism-lazyman-config
     "com.apple.keylayout.ABC" ;; 英文输入法
     "com.apple.inputmethod.SCIM.ITABC")) ;; 拼音输入法
  ;;https://github.com/daipeihust/im-select
  ;;只能切换不同语言的输入法，拼音输入法的中英文切换无法识别
  (with-system windows-nt
    (sis-ism-lazyman-config
     "1033" ;; 英文输入法
     "2052" ;; 拼音输入法
     'im-select)
    )
  (setq sis-prefix-override-keys (list "C-c" "C-x" "C-h"
                                       ;; avy & consult
                                       "M-g" "C-。" "M-s"
                                       ;; easy-kill
                                       ))
  ;; enable the /cursor color/ mode
  (sis-global-cursor-color-mode t)
  ;; enable the /respect/ mode
  (sis-global-respect-mode t)
  ;; enable the /context/ mode for all buffers
  (sis-global-context-mode t)
  ;; enable the /inline english/ mode for all buffers
  (sis-global-inline-mode t)
  (setq sis-inline-tighten-head-rule 0)
  ;;(setq sis-inline-tighten-tail-rule 0)
  )
;; End
