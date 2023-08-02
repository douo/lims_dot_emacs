 ;; -*- lexical-binding: t; -*-
;;; init.el --- douo's emacs config
;;; Commentary:
;;
;;; 个人用

;; disable first narrow hint
(put 'narrow-to-region 'disabled nil)

;; Move customization variables to sparate file
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; gc 优化
;; https://emacs-china.org/t/topic/5720/10
(setq emacs-start-time (float-time))
(setq gc-cons-threshold 10000000)

;; 正确处理 CJK 字符的自动断行
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=29364#11
;; https://emacs-china.org/t/topic/2616/18
(setq word-wrap-by-category t)

(add-hook
 'after-init-hook
 (lambda ()
   (setq gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value)))
   (insert (format ";; Emacs started in %fs\n"
                   (- (float-time) emacs-start-time)))))
;; https://akrl.sdf.org/
;; http://blog.lujun9972.win/blog/2019/05/16/%E4%BC%98%E5%8C%96emacs%E7%9A%84%E5%9E%83%E5%9C%BE%E6%90%9C%E9%9B%86%E8%A1%8C%E4%B8%BA/index.html
(defmacro k-time (&rest body)
  "Measure and return the time it takes evaluating BODY."
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))

(defvar k-gc-timer
  (run-with-idle-timer 15 t
                       (lambda ()
                         (message "Garbage Collector has run for %.06fsec"
                                  (k-time (garbage-collect))))))
;; gc end

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
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(with-system windows-nt
  (setq w32-pass-rwindow-to-system nil)
  (setq w32-rwindow-modifier 'super) ; Right Windows key
  (w32-register-hot-key [s-]))


;; 自动加载外部修改过的文件，如果当前 buffer 未修改
;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode 1)
;; 禁止 Emacs 自动生成备份文件，例如 init.el~ 。
(setq make-backup-files nil)
;; the toolbar/menubar/scrollbar is just a waste of valuable screen estate
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

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
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)


(require 'minibuffer)

(setq use-package-verbose t)

;;On OS X (and perhaps elsewhere) the $PATH environment variable and
;; `exec-path' used by a windowed Emacs instance will usually be the
;; system-wide default path, rather than that seen in a terminal window.
(use-package exec-path-from-shell
  :straight t
  :config
  (when (or (memq window-system '(mac ns x)) (daemonp))
    (dolist (var '("RG_EXECUTABLE" "WRITING_HOME" "GTD_HOME" ;; personal
                   "CONDA_EXE" ;; conda
                   ))
      (add-to-list 'exec-path-from-shell-variables var))
    (exec-path-from-shell-initialize)))


(use-package load-relative
  :straight t)
;; 加载本机特殊配置，环境变量等...
(load-relative "local.el")

(use-package nerd-icons
  :straight t
  :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  (nerd-icons-font-family "Symbols Nerd Font Mono")
  )

(use-package nerd-icons-dired
  :straight t
  :hook
  (dired-mode . nerd-icons-dired-mode))

;; https://github.com/akermu/emacs-libvterm
(if (not IS-WINDOWS)
    (progn
      (use-package vterm
        :straight t)
      (use-package multi-vterm
        :straight t
        :init
        (transient-define-prefix multi-vterm-transient ()
          "Multi vterm transient"
          ["Multi vterm"
           ("c" "Create new terminal" multi-vterm)
           ("n" "Switch to next terminal" multi-vterm-next)
           ("p" "Switch to next terminal" multi-vterm-prev)
           ("t" "Toggle dedicated terminal" multi-vterm-dedicated-toggle)
           ("g" "Create/toggle terminal based on current project" multi-vterm-project)
           ]
          )
        :bind (
               ("s-t" . multi-vterm)
               ("C-c M-t" . multi-vterm-transient))
        )))



;; Library for converting first letter of Pinyin to Simplified/Traditional Chinese characters.
(use-package pinyinlib
  :straight t)


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
;; 类似 smartparens paredit fingertip
(use-package elec-pair
  :config
  (electric-pair-mode +1))


;;光标移动方案
;; https://github.com/abo-abo/avy
(use-package avy
  :straight t
  :bind
  ("M-g w" . avy-goto-word-or-subword-1)
  ("M-g c" . avy-goto-char)
  :config
  (setq avy-background t))
;; avy 支持拼音
(use-package ace-pinyin
  :straight t
  :config
  ;;(setq ace-pinyin-treat-word-as-char nil)
  (setq ace-pinyin-simplified-chinese-only-p nil)
  (ace-pinyin-global-mode +1)
  )
;; git
(use-package magit
  :straight t
  :bind (("C-x g" . magit-status)))
(use-package git-timemachine
  :straight t
  :bind (("M-g t" . git-timemachine)))

;; rg
(use-package rg
  :straight t
  :config
  (rg-enable-default-bindings))

;; https://github.com/magnars/expand-region.el
(use-package expand-region
  :straight t
  :bind ("C-=" . er/expand-region))
;; 更强大的 kill&yank
;; 代替 expand-region?
(use-package easy-kill
  :straight t
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill))

;; 显示匹配数量
(use-package anzu
  :straight t
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode))

;; 移动整个选择文本
(use-package move-text
  :straight t
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift down)] . move-text-down)))

;; 用不同颜色区别嵌套的括号引号等
(use-package rainbow-delimiters
  :straight t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  )

;; 代码中的颜色值可视化
(use-package rainbow-mode
  :straight t
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode))

;; Virtual comments
;; 为文件增加注释，与文件保存分离
(use-package virtual-comment
  :straight t
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
  :straight t
  :config
  (volatile-highlights-mode +1))

;; Save Emacs buffers when they lose focus
(use-package super-save
  :straight t
  :config
  (super-save-mode +1))

;; 旋转 frame 布局
(use-package transpose-frame
  :straight t
  )


;; A Collection of Ridiculously Useful eXtensions for Emacs
(use-package crux
  :straight t
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
  :straight t
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

(use-package corfu
  :straight t
  :custom
  ;; :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; 配合 eglot
  ;; https://github.com/minad/corfu/wiki#configuring-corfu-for-eglot
  ;; (completion-category-overrides '((eglot (styles orderless))))
  ;;:init
  ;;(global-corfu-mode)
  )

(use-package cape
  :straight t
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  :after corfu
  )

(use-package ace-window
  :straight t
  :config
  (global-set-key (kbd "s-w") 'ace-window)
  (global-set-key [remap other-window] 'ace-window))


;; alternative to the built-in Emacs help that provides much more contextual information.
(use-package helpful
  :straight t
  :bind
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)
  )

;; Enable vertico
(use-package vertico
  :straight t
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
;; 可以实现访问越频繁的项越靠前
(use-package savehist
  :straight t
  :init
  (savehist-mode))

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  :after vertico
  :straight t
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
  :straight t
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

  ;; Removed   ;; https://github.com/minad/consult/issues/567
  ;; (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

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
   :preview-key "M-.")

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
  ;; 用于 tui , corfu fallback 到 completion-in-region
  (completion-in-region-function
   (lambda (&rest args)
     (apply (if vertico-mode
                #'consult-completion-in-region
              #'completion--in-region)
            args)))
  )

(use-package embark
  :straight t

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
  :straight t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package orderless
  :straight t
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
  :straight t
  :config
  (which-key-mode))

;; temporarily highlight changes from yanking, etc
(use-package volatile-highlights
  :straight t
  :config
  (volatile-highlights-mode +1))

;; 使用叠加层高亮符号
(use-package symbol-overlay
  :straight t
  :init
  (transient-define-prefix symbol-overlay-transient ()
    "Symbol Overlay transient"
    ["Symbol Overlay"
     ["Overlays"
      ("." "Add/Remove at point" symbol-overlay-put)
      ("k" "Remove All" symbol-overlay-remove-all)
      ]
     ["Move to Symbol"
      ("n" "Next" symbol-overlay-switch-forward)
      ("p" "Previous" symbol-overlay-switch-backward)
      ]
     ["Other"
      ("m" "Highlight symbol-at-point" symbol-overlay-mode)
      ]
     ]
    )
  :bind
  (("s-." . 'symbol-overlay-transient))
  )

(use-package flymake
  :straight t)

;; lsp-bridge
(use-package posframe
  :straight t
  )

(use-package yasnippet
  :straight t
  :config
  (yas-global-mode 1)
  )

(use-package markdown-mode
  :straight t
  )

;; treesit start
;; read https://blog.markhepburn.com/posts/experimenting-with-the-built-in-treesitter-support-in-emacs/
;; download os relate module from: https://github.com/emacs-tree-sitter/tree-sitter-langs
(when (and (not (version< emacs-version "29")) (treesit-available-p))
  (setq treesit-extra-load-path '(concat (file-name-directory user-init-file) "tree-sitter"))
  (add-to-list 'major-mode-remap-alist
               '(c-mode . c-ts-mode)
               '(c++-mode . c++-ts-mode)
               '(python-mode . python-ts-mode)
               )
  )

(use-package treesit-auto
  :straight t
  :config
  (global-treesit-auto-mode))

;; (use-package fingertip
;;   :straight nil
;;   :load-path  "lisp/fingertip"
;;   :hook
;;   (c-mode-common . fingertip-mode)
;;   (c-mode . fingertip-mode)
;;   (c++-mode . fingertip-mode)
;;   (java-mode . fingertip-mode)
;;   (haskell-mode . fingertip-mode)
;;   (emacs-lisp-mode . fingertip-mode)
;;   (lisp-interaction-mode . fingertip-mode)
;;   (lisp-mode . fingertip-mode)
;;   (maxima-mode . fingertip-mode)
;;   (ielm-mode . fingertip-mode)
;;   (sh-mode . fingertip-mode)
;;   (makefile-gmake-mode . fingertip-mode)
;;   (php-mode . fingertip-mode)
;;   (python-mode . fingertip-mode)
;;   (js-mode . fingertip-mode)
;;   (go-mode . fingertip-mode)
;;   (qml-mode . fingertip-mode)
;;   (jade-mode . fingertip-mode)
;;   (css-mode . fingertip-mode)
;;   (ruby-mode . fingertip-mode)
;;   (coffee-mode . fingertip-mode)
;;   (rust-mode . fingertip-mode)
;;   (rust-ts-mode . fingertip-mode)
;;   (qmake-mode . fingertip-mode)
;;   (lua-mode . fingertip-mode)
;;   (swift-mode . fingertip-mode)
;;   (web-mode . fingertip-mode)
;;   (markdown-mode . fingertip-mode)
;;   (llvm-mode . fingertip-mode)
;;   (conf-toml-mode . fingertip-mode)
;;   (nim-mode . fingertip-mode)
;;   (typescript-mode . fingertip-mode)
;;   (c-ts-mode . fingertip-mode)
;;   (c++-ts-mode . fingertip-mode)
;;   (cmake-ts-mode . fingertip-mode)
;;   (toml-ts-mode . fingertip-mode)
;;   (css-ts-mode . fingertip-mode)
;;   (js-ts-mode . fingertip-mode)
;;   (json-ts-mode . fingertip-mode)
;;   (python-ts-mode . fingertip-mode)
;;   (bash-ts-mode . fingertip-mode)
;;   (typescript-ts-mode . fingertip-mode)
;;   :bind  (:map fingertip-mode-map
;;                ("(" . fingertip-open-round)
;;                ("[" . fingertip-open-bracket)
;;                ("{" . fingertip-open-curly)
;;                (")" . fingertip-close-round)
;;                ("]" . fingertip-close-bracket)
;;                ("}" . fingertip-close-curly)
;;                ("=" . fingertip-equal)

;;                ("%" . fingertip-match-paren)
;;                ("\"" . fingertip-double-quote)
;;                ("'" . fingertip-single-quote)

;;                ("SPC" . fingertip-space)
;;                ("RET" . fingertip-newline)

;;                ("M-o" . fingertip-backward-delete)
;;                ("C-d" . fingertip-forward-delete)
;;                ("C-k" . fingertip-kill)

;;                ("M-\"" . fingertip-wrap-double-quote)
;;                ("M-'" . fingertip-wrap-single-quote)
;;                ("M-[" . fingertip-wrap-bracket)
;;                ("M-{" . fingertip-wrap-curly)
;;                ("M-(" . fingertip-wrap-round)
;;                ("M-)" . fingertip-unwrap)

;;                ("M-p" . fingertip-jump-right)
;;                ("M-n" . fingertip-jump-left)
;;                ("M-:" . fingertip-jump-out-pair-and-newline)

;;                ("C-j" . fingertip-jump-up))
;;   )

;; treesit end

;; 主模式

;;
(use-package pkgbuild-mode
  :straight t)

;;
(use-package cmake-mode
  :straight t)

;; python

;; (setq douo/python-lsp-server "pylsp")
(setq douo/python-lsp-server "pyright")


;; 只有安装了 conda 才启用
(use-package conda
  :if (and
       (getenv "CONDA_EXE")
       (file-exists-p (getenv "CONDA_EXE")))
  :straight nil
  :after exec-path-from-shell
  :config
  (conda-env-initialize-interactive-shells)
  (conda-env-initialize-eshell)
  (let ((home
         (file-name-directory (directory-file-name (file-name-directory (getenv "CONDA_EXE"))))))
    (setq conda-anaconda-home (expand-file-name home))
    (setq conda-env-home-directory (expand-file-name home))
    )
  )

;; reformat
;; 需要在环境中已经安装 https://github.com/psf/black
(use-package blacken
  :straight t
  :bind
  (:map python-mode-map
        ("C-c M-f" . blacken-buffer)
        )
  )
(use-package cython-mode
  :straight t)


;; typescript
;; web-mode
;; (setq web-mode-markup-indent-offset 2)
;; (setq web-mode-code-indent-offset 2)
;; (setq web-mode-css-indent-offset 2)
(use-package web-mode
  :straight t
  :mode (("\\.js\\'" . web-mode)
	 ("\\.jsx\\'" .  web-mode)
	 ("\\.html\\'" . web-mode))
  :commands web-mode)


(use-package typescript-mode :defer
  :mode
  (("\\.ts\\'" . tsx-ts-mode)
   ("\\.tsx\\'" . tsx-ts-mode)))

(use-package jsonian
  :straight t
  :after so-long
  :custom
  (jsonian-no-so-long-mode))

;; ruby

(use-package ruby-mode
  :straight t
  :custom
  (ruby-insert-encoding-magic-comment nil)
  )

;;provides a REPL buffer connected to a Ruby subprocess.
(use-package inf-ruby
  :straight t
  :after ruby-mode)

(use-package subword-mode
  :hook ruby-mode)

(with-system darwin
  ;; swift
  ;; https://www.reddit.com/r/emacs/comments/115lbrd/finally_got_eglot_to_work_with_sourcekitlsp_in/
  (use-package swift-mode
    :straight t
    :config
    (with-eval-after-load 'eglot
      (add-to-list 'eglot-server-programs
                   '(swift-mode . ("xcrun" "sourcekit-lsp"))))
    )
  )




;; golang
;; 需安装 goimports gopls
(use-package go-mode
  :straight t)
;; end golang

;; Markdown
(use-package markdown-mode
  :straight t
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
  :straight t
  :mode "\\.lua\\'")


(use-package yaml-mode
  :straight t)

(use-package cask-mode
  :straight t)

(use-package nginx-mode
  :straight t
  )

;; openwrt uci config file
(use-package uci-mode
  :init
  (load-relative "lisp/uci-mode.el")
  :straight `(uci-mode :type git :host github :repo "jkjuopperi/uci-mode")
  )

(load-relative "org.el")

;; 翻译
(use-package go-translate
  :straight t
  :custom
  (gts-translate-list '(("en" "zh")))
  (gts-default-translator
   (gts-translator
    :picker (gts-prompt-picker)
    :engines (list (gts-bing-engine) (gts-google-engine))
    :render (gts-buffer-render)))
  )

;; 输入法
(use-package sis
  :straight t
  :config
  ;; macos
  ;; 使用系统输入法
  ;; 需要先安装 https://github.com/laishulu/macism
  (with-system darwin
    (sis-ism-lazyman-config
     "com.apple.keylayout.ABC" ;; 英文输入法
     "com.apple.inputmethod.SCIM.ITABC" 'macism)  ;; 拼音输入法
    )
  ;;https://github.com/daipeihust/im-select
  ;;只能切换不同语言的输入法，拼音输入法的中英文切换无法识别
  (with-system windows-nt
    (sis-ism-lazyman-config
     "1033" ;; 英文输入法
     "2052" ;; 拼音输入法
     'im-select)
    )
  (with-system gnu/linux
    (message "linux")
    ;; ibus
    ;; (sis-ism-lazyman-config "xkb:us::eng" "libpinyin" 'ibus)
    ;; fcitx5
    (sis-ism-lazyman-config "1" "2" 'fcitx5)
    )
  ;; hack start
  ;; emacs 启动的时候 sis-global-respect-mode 调用 sis--ensure-ism 导致 sis--ism-inited 被置 t。默认 macos 没问题，其他系统没法正确初始化。
  ;; 手动重置一下
  (setq sis--ism-inited nil)
  (sis-global-respect-mode)
  ;; hack end
  :custom
  (sis-prefix-override-keys (list "C-c" "C-x" "C-h"
                                  ;; avy & consult
                                  "M-g" "C-。" "M-s"
                                  ;; easy-kill
                                  ))
  ;; enable the /cursor color/ mode
  (sis-global-cursor-color-mode t)
  (sis-default-cursor-color "white")
  (sis-other-cursor-color "orange")
  ;; enable the /respect/ mode
  (sis-global-respect-mode t)
  ;; enable the /context/ mode for all buffers
  (sis-global-context-mode t)
  ;; enable the /inline english/ mode for all buffers
  (sis-global-inline-mode t)
  (sis-inline-tighten-head-rule 0)
  ;;(setq sis-inline-tighten-tail-rule 0)
  )
;; End

;; root 权限打开文件
;; 在 dired 配合 embark 使用最佳
;; https://emacs.stackexchange.com/a/17726/30746
(use-package tramp
  :config
  (defun sudo-find-file (file)
    "Opens FILE with root privileges."
    (interactive "FFind file: ")
    (set-buffer
     (find-file (concat "/sudo::" (expand-file-name file)))))
  (defun sudo-remote-find-file (file)
    "Opens repote FILE with root privileges."
    (interactive "FFind file: ")
    (setq begin (replace-regexp-in-string  "scp" "ssh" (car (split-string file ":/"))))
    (setq end (car (cdr (split-string file "@"))))
    (set-buffer
     (find-file (format "%s" (concat begin "|sudo:root@" end)))))
  )


;; 在 mode-line 显示时间
(display-time-mode)

;; tui/gui 切换不同配置，主要是切换 lsp-bridge 和 eglot
(if (display-graphic-p)
    ;;(load-relative "gui.el")
    (load-relative "tui.el")
  (load-relative "tui.el")
  )

;; macOS Fix
(with-system darwin
  ;; 规避 macOS child-frame 全屏黑屏
  ;; https://emacs-china.org/t/mac/11848/8
  (if (featurep 'cocoa)
      (progn
        (setq ns-use-native-fullscreen nil)
        (setq ns-use-fullscreen-animation nil)

        (set-frame-parameter (selected-frame) 'fullscreen 'maximized)

        (run-at-time "2sec" nil
                     (lambda ()
                       (toggle-frame-fullscreen)
                       )))
    (require 'fullscreen)
    (fullscreen))

  )
