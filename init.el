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

(when (not (version< emacs-version "29"))
  ;; https://www.masteringemacs.org/article/whats-new-in-emacs-29-1?utm_source=newsletter&utm_medium=rss#:~:text=C%2Dc%20j%20might%20be%20good.
  (keymap-global-set "C-c j" #'duplicate-dwim)
  ;; sqlite-mode 不能通过 find-file 直接打开，需要通过 sqlite-mode-open-file
  (keymap-global-set "C-x t s" #'sqlite-mode-open-file)

  (custom-set-variables
   ;; 如果光标在一个闭合分隔符内且开放分隔符不在屏幕上显示，则在回显区域显示开放分隔符周围的一些上下文。默认值为nil。
   '(show-paren-context-when-offscreen 'child-frame)
   ;; flymake mode-line prefix
   '(flymake-mode-line-lighter " ")
   ;; 进程列表(proced)显示颜色
   '(proced-enable-color-flag 't)
   )
  )


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

(straight-use-package 'org)
(straight-use-package 'use-package)
;; 提供简单的方法修改 minor-mode 在 modeline 中的 indicator
(straight-use-package 'diminish)
;; 与 diminish 不兼容
(use-package minions
  :straight t
  :config
  (minions-mode 1)
  ;; (add-to-list 'minions-promoted-modes 'flymake-mode)
  ;; 不隐藏 flymake-mode 的 indicator
  (add-to-list 'minions-prominent-modes 'flymake-mode)
  )


;;
(use-package benchmark-init
  :straight t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

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
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; set font-family to `Hack Nerd Font Mono' if it exist in (font-family-list)
  :config
  ;; NL 表示 no-ligatures 即没有使用连字，保留了字符的原始样式。这样的变体通常在代码编辑器和终端中更具有可读性，因为它们保留了字符的独特形状。
  ;; Mono 变体: "Mono" 变体意味着该字体是等宽字体，适用于代码编辑器和终端。每个字符的宽度相同，从而确保代码的对齐和格式化保持一致，提高了代码的可读性。
  ;; Propo 变体: "Propo" 变体意味着该字体是比例字体，适用于文本编辑器和图形应用程序。每个字符的宽度不同，从而确保文本的对齐和格式化保持一致，提高了文本的可读性。
  ;; https://github.com/ryanoasis/nerd-fonts/discussions/1103
  (if (member "JetBrainsMonoNL Nerd Font Propo" (font-family-list))
      (setq nerd-icons-font-family "JetBrainsMonoNL Nerd Font Propo")
    (if (member "Hack Nerd Font Mono" (font-family-list))
        (setq nerd-icons-font-family "Hack Nerd Font Mono")
      )
    )
  ;; temporary fix for https://github.com/rainstormstudio/nerd-icons.el/issues/29
  (setf (alist-get 'benchmark-init/tree-mode nerd-icons-mode-icon-alist) '(nerd-icons-faicon "nf-fa-dashboard"))
  )

(use-package nerd-icons-dired
  :straight t
  :hook
  (dired-mode . nerd-icons-dired-mode))

;; 提供 minibuf 补全的图标（文件）
(use-package nerd-icons-completion
  :straight t
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

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
  :diminish "拼"
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
  (keymap-global-set "<remap> <kill-ring-save>" 'easy-kill))

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
  :diminish  "括";;"  " ;; "🌈"
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode))

;; 空格可视化
(use-package whitespace
  :init
  (dolist (hook '(prog-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :diminish "空";;"  "
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
  :diminish "存";;"  "
  :config
  (super-save-mode +1))

;; 旋转 frame 布局
(use-package transpose-frame
  :straight t
  )

;; 用于上传文本、文件到 https://0x0.st/ 或其它短链接分享服务
;; 可自建 https://git.0x0.st/mia/0x0
(use-package 0x0
  :straight t
  :custom
  ;; 0x0 屏蔽了整个谷歌云 IP 段
  (0x0-default-server 'ttm)
  )


;; 翻译
(use-package go-translate
  :straight t
  :custom
  (gts-translate-list '(("en" "zh")))
  (gts-default-translator
   (gts-translator
    :picker (gts-prompt-picker)
    :engines (list (gts-google-rpc-engine))
    :render (gts-buffer-render)
    ;; :splitter (gts-paragraph-splitter)
    )
   )
  )


;; begin_epub
(use-package nov
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  )

;; TODO
;; - [ ] search
;; - [ ] selection
(use-package nov-xwidget
  :straight `(nov-xwidget :type git :host github :repo "chenyanming/nov-xwidget")
  :demand t
  :after nov
  :config
  (define-key nov-mode-map (kbd "o") 'nov-xwidget-view)
  (add-hook 'nov-mode-hook 'nov-xwidget-inject-all-files)
  :bind
  (:map nov-xwidget-webkit-mode-map
        ("n" . nov-xwidget-next-document)
        ("p" . nov-xwidget-previous-document)
        ("t" . nov-xwidget-goto-toc)
        )
  )
;; end_epub

;; begin_pdf
(use-package pdf-tools
  :straight t
  :defer t
  :config
  (custom-set-variables
   '(pdf-tools-handle-upgrades nil)) ; Use brew upgrade pdf-tools instead.
  (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo")

  ;; 自定义 pdf 翻译文本提取器
  ;; 如果有高亮返回高亮文本，无则返回整页文本
  (defclass douo/gts-pdf-view-selection-texter (gts-texter) ())
  (cl-defmethod gts-text ((_ douo/gts-pdf-view-selection-texter))
    (unless (pdf-view-active-region-p)
      (pdf-view-mark-whole-page)
      )
    ;; remove-newline-characters-if-not-at-the-end-of-sentence
    ;; ::HACK:: 解决 pdf 提取文本不能正确断行的问题
    ;; 移除不是处于句尾[.!?]的换行符
    (replace-regexp-in-string "\\([^.!?]\\)\n\\([^ ]\\)" "\\1 \\2"
                              (car (pdf-view-active-region-text)))
    )
  (defvar douo/pdf-translater
    (gts-translator
     :picker (gts-noprompt-picker :texter (douo/gts-pdf-view-selection-texter))
     :engines (list (gts-google-rpc-engine))
     :render (gts-buffer-render)
     ;; :splitter (gts-paragraph-splitter)
     )
    )
  (defun douo/pdf-view-translate ()
    (interactive)
    (gts-translate douo/pdf-translater)
    ;;  cancel selection in emacs
    (deactivate-mark)
    )
  ;; 如果没有 epdfinfo，以下命令重新编译
  (pdf-tools-install)
  :bind
  (:map pdf-view-mode-map
        ;; consult 不支持与 pdf-tools 的交互
        ("C-s" . isearch-forward)
        ("C-r" . isearch-backward)
        ("T" . douo/pdf-view-translate)
        )
  )
;; end_pdf

;; A Collection of Ridiculously Useful eXtensions for Emacs
(use-package crux
  :straight t
  :bind (("C-c o" . crux-open-with)
         ("C-c N" . crux-cleanup-buffer-or-region)
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
         ([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ;; ("M-o" . crux-smart-open-line)
         ([(shift return)] . crux-smart-open-line)
         ;; ("s-o" . crux-smart-open-line-above)
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

(use-package eldoc
  :diminish "显"
  )

;;
;; 代码补完前端，当前位置代码补完，弹出补全菜单。
(use-package corfu
  ;; straight hack. 加载扩展
  :straight (:files (:defaults "extensions/*"))
  :custom
  ;; :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto nil)                 ;; Enable auto completion
  ;; orderless 是一种高级补完 style
  ;; 通过 spearator 划分过滤关键字
  (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  :init
  (global-corfu-mode)
  :config
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (when completion-in-region--data
      (let ((completion-extra-properties corfu--extra)
            completion-cycle-threshold completion-cycling)
        (apply #'consult-completion-in-region completion-in-region--data))))
  (keymap-set corfu-map "M-m" #'corfu-move-to-minibuffer)
  (add-to-list 'corfu-continue-commands #'corfu-move-to-minibuffer)
  :bind
  (:map corfu-map
        ("SPC" . corfu-insert-separator)
        ("C-M-m" . corfu-move-to-minibuffer)
        )
  )

;; M-g 跳转候选位置
;; M-h 显示候选文档
;; 自动显示可用 corfu-popupinfo
;; 通过 corfu 引入无需再手动 straight
(use-package corfu-info
  :after corfu)

(use-package kind-icon
  :straight t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; 提供补完后端 capfs(completion-at-point-functions)
;; 能将 company 后端转换为 capfs
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
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  ;; NOTE: The order matters!
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;;(add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )


(use-package ace-window
  :straight t
  :custom
  (aw-dispatch-always t)
  ;; 搭配使用，通过 minibuf 提示当前处于 ace-window 模式
  (aw-background nil)
  (aw-minibuffer-flag t)

  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind  (
          ("M-o" . ace-window)
          )
  )

;; 内置的 winner-mode 可以记忆窗口布局
(use-package winner-mode
  :config
  (winner-mode 1)
  :bind (
         ;; 回退窗口布局
         ("M-S-<left>" . winner-undo)
         ("M-S-<right>" . winner-redo)
         ;; 与 ace-window 重复
         ("M-<right>" . windmove-right)
         ("M-<left>" . windmove-left)
         ("M-<up>" . windmove-up)
         ("M-<down>" . windmove-down)
         )
  )



;; alternative to the built-in Emacs help that provides much more contextual information.
(use-package helpful
  :straight t
  :bind
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)
  )

;; start vertico
;; minibuffer completion UIs
(use-package vertico
  :straight (:files (:defaults "extensions/*"))
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-count 10)
  )
;; extensions
;; 在普通 buffer 而不是 minibuffer 中显示候选项
(use-package vertico-buffer
  :after vertico
  )

(use-package vertico-reverse
  :after vertico
  )

(use-package vertico-multiform
  :after vertico
  :init
  (vertico-multiform-mode +1)
  :config
  ;; bind 可以 vertico buffer 中切换显示方式
  ;; M-V -> vertico-multiform-vertical
  ;; M-G -> vertico-multiform-grid
  ;; M-F -> vertico-multiform-flat
  ;; M-R -> vertico-multiform-reverse
  ;; M-U -> vertico-multiform-unobtrusive
  )




;; Persist history over Emacs restarts. Vertico sorts by history position.
;; 可以实现访问越频繁的项越靠前
(use-package savehist
  :straight t
  :init
  (savehist-mode))

;; 为 minibuffer 候选项提供更多信息(旁注)
;; Marginalia 连接到 Emacs 补全框架并运行变量 `marginalia-classifiers' 中列出的分类器，这些分类器使用命令的提示符或候选项的其他属性来指定补全类别。
;; 一旦知道候选者的类别，Marginalia 就会在 `marginalia-annotator-registry' 中查找要使用的关联注释器。注释器是一个函数，它将完成候选字符串作为参数，并返回要在迷你缓冲区中的候选后面显示的注释字符串。
(use-package marginalia
  :after vertico
  :straight t
  ;; 切换关联注释器(annotator)
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))



;; A few more useful configurations...
;; 来自 https://github.com/minad/vertico#configuration
(use-package emacs
  :init
  ;; 在提示语中显示 crm(多选补完) 的分隔符
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible nil face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))
;; end vertico


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
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c m" . consult-man)
         ("C-c k" . consult-kmacro)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
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
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ;; 异步搜索
         ;; 至少 `consult-async-min-input' 个字符后，才调用搜索。
         ;; 第一个字符是标点符号，例如 # （默认），Consult 会将输入字符串分成两部分。例如 #regexps#filter-string ，在第二个 # 处拆分
         ;; regexps 交给 grep ，filter-string 则在 emacs 这边过滤
         ;; 更多例子
         ;; `#defun': Search for “defun” using grep.
         ;; `#consult embark': Search for both “consult” and “embark” using grep in any order.
         ;; `#first.*second': Search for “first” followed by “second” using grep.
         ;; `#\(consult\|embark\)': Search for “consult” or “embark” using grep. Note the usage of Emacs-style regular expressions.
         ;; `#defun#consult': Search for “defun” using grep, filter with the word “consult”.
         ;; `/defun/consult': It is also possible to use other punctuation characters.
         ;; `#to#': Force searching for “to” using grep, since the grep pattern must be longer than `consult-async-min-input' characters by default.
         ;; `#defun -- --invert-match#': Pass argument --invert-match to grep.
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
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)


  ;; begin_isearch_like
  ;; 实现类似 isearch 的 C-s C-r 行为
  ;; modified from https://github.com/minad/consult/wiki#isearch-like-backwardforward-consult-line
  (defun consult-line-wrapper (func)
    "Search for a matching line forward."

    (let* (
           ;; 翻转 vertico-reverse
           (next (if (eq func 'consult-line)
                     #'vertico-next
                   #'vertico-previous
                   ))
           (prev (if (eq func 'consult-line)
                     #'vertico-previous
                   #'vertico-next
                   ))
           (bind-hook (lambda ()
                        (keymap-local-set "C-s" next)
                        (keymap-local-set "C-r" prev)
                        ))
           (unbind-hook (lambda ()
                          ;; HACK 手动指定 keymap
                          ;; `minibuffer-setup-hook' 是作用于 `vertico-map'
                          ;; `minibuffer-exit-hook' 的时候已经恢复为 `minibuffer-local-map' 所以直接 unset 不能生效
                          (use-local-map vertico-map)
                          (keymap-local-unset "C-s")
                          (keymap-local-unset "C-r")
                          ))
           )
      (add-hook 'minibuffer-setup-hook bind-hook)
      (add-hook 'minibuffer-exit-hook unbind-hook)
      (unwind-protect (funcall func)
        (remove-hook 'minibuffer-setup-hook bind-hook)
        (remove-hook 'minibuffer-exit-hook unbind-hook)
        ))
    )
  (defun my/consult-line-forward ()
    "Search for a matching line forward."
    (interactive)
    (consult-line-wrapper 'consult-line)
    )

  (defun my/consult-line-backward ()
    "Search for a matching line backward."
    (interactive)
    (consult-line-wrapper (lambda ()
                            (advice-add 'consult--line-candidates :filter-return 'reverse)
                            (vertico-reverse-mode +1)
                            (unwind-protect (consult-line)
                              (vertico-reverse-mode -1)
                              (advice-remove 'consult--line-candidates 'reverse))))
    )
  (with-eval-after-load 'consult
    (consult-customize my/consult-line-backward
                       :prompt "Go to line backward: ")
    (consult-customize my/consult-line-forward
                       :prompt "Go to line forward: "))

  (keymap-global-set "C-s" 'my/consult-line-forward)
  (keymap-global-set "C-r" 'my/consult-line-backward)
  ;; end_isearch_like

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; 预览
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  ;; 主题延迟自动预览
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   ;; 手动快捷键触发
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key "M-.")


  ;; narrow 缩小候选范围
  ;; 逆操作是`consult-widen-key'
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; begin_vertico_multiform
  ;; Configure the display per command.
  (setq vertico-multiform-commands
        ;; Use a buffer with indices for imenu and consult-grep
        `((consult-imenu buffer indexed)
          ;; Configure `consult-outline' as a scaled down TOC in a separate buffer
          (consult-outline buffer ,(lambda (_) (text-scale-set -1)))
          (consult-grep buffer)
          ))
  ;; `vertico-multiform-categories' Configure the display per completion category.
  (setq vertico-multiform-categories
        ;; 将当前窗口重用于 consult-grep 类别（ consult-grep 、 consult-git-grep 和 consult-ripgrep ）的命令
        `((consult-grep
           buffer
           (vertico-buffer-display-action . (display-buffer-same-window)))))
  ;; Disable preview for consult-grep commands
  (consult-customize consult-ripgrep consult-git-grep consult-grep :preview-key nil)
  ;; end_vertico_multiform

  ;; :custom
  ;; 用于 tui , corfu fallback 到 completion-in-region
  ;; 过时 obsolete，用 `corfu-terminal' 代替
  ;; (completion-in-region-function
  ;;  (lambda (&rest args)
  ;;    (apply (if vertico-mode
  ;;               #'consult-completion-in-region
  ;;             #'completion--in-region)
  ;;           args)))
  )

;; 为当前目标提供 context action, 每个 category 都有一份 action
;; 保存在变量里，比如文件对应的是 `embark-file-map'
;; 支持多选，通过 `embark-select'(SPC) 选择，通过 `embark-act-all'(A) 执行
;; `embark-export'/`embark-collect' 进入 *特定*/embark-collection-mode(fallback) buffer 处理当前候选项
;; `embark-act-all' `embark-export' 和 `embark-collect' 优先临时目标列表。
;; 若临时目标列表为空，在迷你缓冲区中，它们对所有当前完成候选进行操作，或者在 Dired 缓冲区中，它们对所有标记的文件（或所有文件，如果没有标记）进行操作。
(use-package embark
  :straight t
  :after (ace-window)
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; 通过 Eldoc 显示 Embark 目标。
  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; 调整 Eldoc 策略，如果您想从多个提供者那里看到文档。
  ;;(setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ;; 默认行为是 `xref-find-definitions'(M-.)
   ;; 用 `embark-dwim' 提供更多功能性
   ("M-." . embark-dwim)        ;; good alternative: C-;
   ("C-h B" . embark-bindings) ;; alternative for `describe-bindings'
   ("C-;" . embark-act-noquit))
  (:map embark-collect-mode-map
        ("m" . embark-select)
        )
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  ;; begin_ace_window
  ;; 配合 ace-window 指定 window 打开目标
  (eval-when-compile
    (defmacro my/embark-ace-action (fn)
      `(defun ,(intern (concat "my/embark-ace-" (symbol-name fn))) ()
         (interactive)
         (with-demoted-errors "%s"
           (let ((aw-dispatch-always t))
             (aw-switch-to-window (aw-select nil))
             (call-interactively (symbol-function ',fn)))))))
  (define-key embark-file-map     (kbd "o") (my/embark-ace-action find-file))
  (define-key embark-buffer-map   (kbd "o") (my/embark-ace-action switch-to-buffer))
  (define-key embark-bookmark-map (kbd "o") (my/embark-ace-action bookmark-jump))
  ;; end_ace_window

  ;; begin_0x0
  (define-key embark-region-map   (kbd "U") '0x0-dwim)
  (define-key embark-file-map   (kbd "U") '0x0-dwim)
  ;; end_0x0

  ;; begin_sudo_find_file
  ;; root 权限打开文件
  (defun sudo-find-file (file)
    "Open FILE as root."
    (interactive "FOpen file as root: ")
    (when (file-writable-p file)
      (user-error "File is user writeable, aborting sudo"))
    (if (file-remote-p file)
        (let ((begin (replace-regexp-in-string  "scp" "ssh" (car (split-string file ":/"))))
              (end (car (cdr (split-string file "@")))))
          (set-buffer
           (find-file (format "%s" (concat begin "|sudo:root@" end))))
          )
      ;; local file
      (set-buffer
       (find-file (concat "/sudo::" (expand-file-name file)))
       )))
  (define-key embark-file-map (kbd "S") (my/embark-ace-action sudo-find-file))
  ;; end_sudo_find_file

  ;; :custom
  ;; 因为 `C-u C-.' 能实现 noquit  所以这里不需要了
  ;; (embark-quit-after-action nil) ;; 执行操作后不退出 minibuffer，默认是 t
  )

;;
(use-package consult-dir
  :straight t
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :straight t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; 将模式划分为空格分隔的组件，并匹配以任何顺序匹配所有组件的候选者。
;; 组件默认启用正则表达式和文字匹配。
;; 作用于 Completion 后端，与任意前端（corfu vertico）搭配使用
(use-package orderless
  :straight t
  :after (pinyinlib)
  :preface
  ;; https://emacs-china.org/t/vertico/17913/3
  (defun completion--regex-pinyin (str)
    (orderless-regexp (pinyinlib-build-regexp-string str)))
  :config
  (add-to-list 'orderless-matching-styles 'completion--regex-pinyin)
  :custom
  ;; 见 https://github.com/minad/vertico#tramp-hostname-and-username-completion
  ;; 修复 < emacs 30 `/sshx:` 无法补完主机名和用户名
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion))))
  )
;; end vertico

(use-package which-key
  :straight t
  :diminish "键";;"  "
  :config
  (which-key-mode))

;; temporarily highlight changes from yanking, etc
(use-package volatile-highlights
  :straight t
  :diminish "闪"
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
  :straight t
  )

(use-package flymake-relint
  :straight `(flymake-relint :type git :host github :repo "liuyinz/flymake-relint")
  :hook
  (emacs-lisp-mode . flymake-relint-setup)
  (lisp-interaction-mode . flymake-relint-setup)
  )

;; lsp-bridge
(use-package posframe
  :straight t
  )

(use-package yasnippet
  :straight t
  :config
  (yas-global-mode 1)
  (diminish 'yas-minor-mode "ⓨ")
  )

(use-package yasnippet-snippets
  :straight t
  :after yasnippet
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
  :straight t
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
  :straight `(uci-mode :type git :host github :repo "jkjuopperi/uci-mode")
  )

(load-relative "org.el")

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
                                  ;; ace-window
                                  "M-o"
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


;; 在 mode-line 显示时间
(use-package time
  :custom
  (display-time-format "%H:%M:%S")
  (display-time-24hr-format 1)
  (display-time-interval 1)
  :config
  (display-time-mode 1)
  )


;; tui/gui 切换不同配置，主要是切换 lsp-bridge 和 eglot
(if (display-graphic-p)
    ;;(load-relative "gui.el")
    (load-relative "tui.el")
  (load-relative "tui.el")
  )

;; macOS Fix
;; FIXME to_be_deleted
;; 交给 wm(yabai) 处理
;; (with-system darwin
;;   ;; 规避 macOS child-frame 全屏黑屏
;;   ;; https://emacs-china.org/t/mac/11848/8
;;   (if (featurep 'cocoa)
;;       (progn
;;         (setq ns-use-native-fullscreen nil)
;;         (setq ns-use-fullscreen-animation nil)

;;         (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
;;         (menu-bar-mode t)
;;         (run-at-time "2sec" nil
;;                      (lambda ()
;;                        (toggle-frame-fullscreen)
;;                        )))
;;     (require 'fullscreen)
;;     (fullscreen))

;;   )

;; * copilot
(setq douo/copilot-provider `copilot)
;; switch...case by douo/copilot-provider
(cl-case douo/copilot-provider
  ;; ** Copilot
  (`copilot
   (use-package copilot
     :straight '(:type git :host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
     :init
     ;; accept completion from copilot and fallback to corfu-complete
     (defun douo/copilot-complete ()
       (interactive)
       (or (copilot-accept-completion)
           (corfu-complete)))
     :hook (prog-mode . copilot-mode)
     :diminish "  "
     :bind
     (:map copilot-completion-map
           ("<tab>" . douo/copilot-complete)
           ("TAB" . douo/copilot-complete)
           ("M-f" . copilot-accept-completion-by-word)
	   ("M-<return>" . copilot-accept-completion-by-line)
           ("M-[" . copilot-previous-completion)
	   ("M-]" . copilot-next-completion)
           ("C-g" . copilot-clear-overlay)
           )
     :config
     (add-to-list 'minions-prominent-modes 'copilot-mode)
     )
   )
  ;; ** Tabnine
  ;; https://github.com/shuxiao9058/tabnine
  (`tabnine
   (use-package tabnine
     :init
     ;; accept completion from copilot and fallback to corfu-complete
     (defun douo/copilot-complete ()
       (interactive)
       (or (copilot-accept-completion)
           (corfu-complete)))
     :commands (tabnine-start-process)
     :hook (prog-mode . tabnine-mode)
     :straight t
     :diminish " ⌬ "
     :custom
     (tabnine-wait 1)
     (tabnine-minimum-prefix-length 0)
     :hook (kill-emacs . tabnine-kill-process)
     :config
     (add-to-list 'completion-at-point-functions #'tabnine-completion-at-point)
     (tabnine-start-process)
     :bind
     (:map  tabnine-completion-map
	    ("M-TAB" . tabnine-accept-completion)
	    ("C-g" . tabnine-clear-overlay)
	    ("M-[" . tabnine-previous-completion)
	    ("M-]" . tabnine-next-completion))))
  (`codeium
   ;; ** Codeium
   ;; https://github.com/codeium/codeium
   ;; https://github.com/Exafunction/codeium.el
   ;; we recommend using use-package to organize your init.el
   (use-package codeium
     ;; if you use straight
     :straight '(:type git :host github :repo "Exafunction/codeium.el")
     ;; otherwise, make sure that the codeium.el file is on load-path

     :init
     ;; use globally
     ;; (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
     ;; or on a hook
     ;; (add-hook 'python-mode-hook
     ;;     (lambda ()
     ;;         (setq-local completion-at-point-functions '(codeium-completion-at-point))))

     ;; if you want multiple completion backends, use cape (https://github.com/minad/cape):
     (add-hook 'python-mode-hook
               (lambda ()
                 (setq-local completion-at-point-functions
                             (list (cape-super-capf #'codeium-completion-at-point #'lsp-completion-at-point)))))
     ;; an async company-backend is coming soon!

     ;; codeium-completion-at-point is autoloaded, but you can
     ;; optionally set a timer, which might speed up things as the
     ;; codeium local language server takes ~0.2s to start up
     ;; (add-hook 'emacs-startup-hook
     ;;  (lambda () (run-with-timer 0.1 nil #'codeium-init)))

     ;; :defer t ;; lazy loading, if you want
     :config
     (setq use-dialog-box nil) ;; do not use popup boxes

     ;; if you don't want to use customize to save the api-key
     ;; (setq codeium/metadata/api_key "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")

     ;; get codeium status in the modeline
     (setq codeium-mode-line-enable
           (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
     (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)
     ;; alternatively for a more extensive mode-line
     ;; (add-to-list 'mode-line-format '(-50 "" codeium-mode-line) t)

     ;; use M-x codeium-diagnose to see apis/fields that would be sent to the local language server
     (setq codeium-api-enabled
           (lambda (api)
             (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))
     ;; you can also set a config for a single buffer like this:
     ;; (add-hook 'python-mode-hook
     ;;     (lambda ()
     ;;         (setq-local codeium/editor_options/tab_size 4)))

     ;; You can overwrite all the codeium configs!
     ;; for example, we recommend limiting the string sent to codeium for better performance
     (defun my-codeium/document/text ()
       (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))
     ;; if you change the text, you should also change the cursor_offset
     ;; warning: this is measured by UTF-8 encoded bytes
     (defun my-codeium/document/cursor_offset ()
       (codeium-utf8-byte-length
        (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
     (setq codeium/document/text 'my-codeium/document/text)
     (setq codeium/document/cursor_offset 'my-codeium/document/cursor_offset))
   )
  (otherwise (message "copilot not set"))
  )
