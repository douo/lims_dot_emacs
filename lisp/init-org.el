;;; init-org.el -- Initialize Org configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Org configurations.
;;

;;; Code:


(require 'init-vars)
(defvar douo/roam-home (concat (file-name-as-directory douo/writing-home) "_roam"))
(defvar  douo/gtd-home (concat (file-name-as-directory douo/roam-home) "_gtd"))

(defun douo/org-gtd-archive ()
  "将 GTD inbox 中的条目归档为参考资料，但不自动跳回 inbox。"
  (interactive)
  ;; org-gtd 4.x 不再需要 with-org-gtd-context，直接局部绑定归档位置即可。
  (let ((org-archive-location (org-gtd--effective-archive-location)))
    (org-archive-subtree-default)))

(defun douo/org-gtd-engage ()
  "显示一个自定义的 GTD 总览 agenda。"
  (interactive)
  ;; 临时定义自定义 agenda 命令，并立即跳转到该视图。
  (let ((org-agenda-custom-commands
     `(("g" "Scheduled today and all NEXT items"
    ((agenda "" ((org-agenda-span 1)
         (org-agenda-start-day nil)
         (org-agenda-skip-additional-timestamps-same-entry t)))
     ;; 4.x 起 GTD 语义状态由 org-gtd-keyword-mapping 决定，不再依赖旧变量。
     ;; 用内置 %b（breadcrumbs）代替 org-get-outline-path 手动调用：
     ;; %b 在 agenda 内批量缓存，不会对每条目反复遍历 outline 树，速度快很多。
     (todo ,(alist-get 'next org-gtd-keyword-mapping)
       ((org-agenda-overriding-header "All NEXT items")
        (org-agenda-prefix-format '((todo . " %i %b")))))
     (todo ,(alist-get 'wait org-gtd-keyword-mapping)
       ((org-agenda-todo-ignore-with-date t)
        (org-agenda-overriding-header "Delegated/Blocked items")
        (org-agenda-prefix-format '((todo . " %i %b"))))))
    ((org-agenda-inhibit-startup t))))))
    (org-agenda nil "g")
    (goto-char (point-min))))

(defun douo/org-agenda-todo-dwim (&optional arg)
  "在 Agenda 中以 toggle 方式切换 GTD 条目的 TODO 状态。

默认情况下，如果当前 agenda 条目的 TODO 状态属于 GTD 的开放态，
则直接切换到 `org-gtd-keyword-mapping' 中定义的 DONE 状态；
如果当前已经是 DONE 或 CNCL，则切回默认开放态 TODO。

带前缀参数 ARG 时，回退到原生 `org-agenda-todo'，保留循环或手动选择
TODO 状态的能力。非 GTD 条目也回退到原生命令。"
  (interactive "P")
  (if arg
      (call-interactively #'org-agenda-todo)
    (let* ((current-state (org-get-at-bol 'todo-state))
           (todo-state (alist-get 'todo org-gtd-keyword-mapping))
           (done-state (alist-get 'done org-gtd-keyword-mapping))
           (canceled-state (alist-get 'canceled org-gtd-keyword-mapping))
           (open-states (delq nil
                              (mapcar (lambda (state)
                                        (alist-get state org-gtd-keyword-mapping))
                                      '(todo next wait)))))
      (cond
       ((member current-state open-states)
        (org-agenda-todo done-state))
       ((member current-state (delq nil (list done-state canceled-state)))
        (org-agenda-todo todo-state))
       (t
        (call-interactively #'org-agenda-todo))))))

(defun douo/generate-quick-note (path)
  "在 PATH 下按日期生成或返回当天 quick note 文件路径。"
  (let ((file
         (expand-file-name (format-time-string "%Y/%m/note-%Y-%m-%d.org") path)))
    (if (f-exists? file)
        file
      (mkdir (f-dirname file) t)
      (f-write-text
       (concat "#+TITLE: " (format-time-string "%Y年%m月%d日杂记") "\n"
               "#+date: " (format-time-string "[%Y-%m-%d]") "\n\n")
       'utf-8 file)
      file)))

(defun douo/insert-header-from-note-name ()
  "从缓冲区名称解析日期，插入笔记的标题和日期 header。"
  (interactive)
  ;; 从缓冲区名称中提取日期部分（substring 5 -4 提取 "note-" 之后、".org" 之前的部分）
  (let ((date (encode-time (org-parse-time-string (substring (buffer-name) 5 -4)))))
    (save-excursion
      (goto-char (point-min))
      ;; 在文件起始处插入标题和日期 header
      (insert (concat "#+TITLE: " (org-format-time-string "%Y年%m月%d日杂记" date nil)  "\n"
                      "#+date: " (format-time-string "[%Y-%m-%d]" date) "\n\n")))))

;; https://emacs.stackexchange.com/a/2559/30746
;; FYI: 手动补全 C-c C-x C-f *
;; FYI: org-emphasize
(defvar org-electric-pairs '((?* . ?*) (?~ . ?~) (?+ . ?+) (?_ . ?_) (?= . ?=))
  "Org 模式中文本强调的电对字符配置。
  支持 *斜体* ~删除线~ +上划线+ _下划线_ =等宽代码= 等语法。")
(defun org-add-electric-pairs ()
  "在当前缓冲区启用 Org 模式文本强调的电对自动补全。

该函数为当前缓冲区的 electric-pair-pairs 和 electric-pair-text-pairs
添加 Org 特定的强调符号，使得输入左符号时会自动添加右符号。
这些对包括 *、~、+、_、= 等 Org 强调标记。"
  (setq-local
   ;; 将 Org 强调对追加到 electric-pair-pairs，实现自动配对
   electric-pair-pairs (append electric-pair-pairs org-electric-pairs))
  (setq-local
   ;; 同步 electric-pair-text-pairs 以确保一致性
   electric-pair-text-pairs electric-pair-pairs))

;; https://xenodium.com/emacs-dwim-do-what-i-mean/
(defun ar/org-insert-link-dwim ()
  "实现智能 Org 链接插入，根据剪切板内容和选区自动处理。

优先级处理逻辑（从高到低）：
1. 若有选中区域且剪切板是 URL：用剪切板 URL 链接选区文本
2. 若仅有剪切板 URL：自动获取 URL 的页面标题作为链接描述
3. 否则：调用标准 `org-insert-link' 交互式输入

这个函数实现了 DWIM（Do What I Mean）理念，减少用户手动输入链接的工作量。"
  (interactive)
  ;; 检查当前光标是否已在 Org 链接内
  (let* ((point-in-link (org-in-regexp org-link-any-re 1))
         ;; 从剪切板获取 URL（若剪切板内容匹配 HTTP 协议则视为有效 URL）
         (clipboard-url (when (string-match-p "^http" (current-kill 0))
                          (current-kill 0)))
         ;; 获取当前选中的文本
         (region-content (when (region-active-p)
                           (buffer-substring-no-properties (region-beginning)
                                                           (region-end)))))
    ;; 根据不同情况执行相应的链接插入策略
    (cond ((and region-content clipboard-url (not point-in-link))
           ;; 情形1：选中文本 + URL 剪切板 + 不在链接内 => 直接用 URL 链接选区文本
           (delete-region (region-beginning) (region-end))
           (insert (org-make-link-string clipboard-url region-content)))
          ((and clipboard-url (not point-in-link))
           ;; 情形2：仅有 URL 剪切板 => 自动获取页面标题并创建链接
           (insert (org-make-link-string
                    clipboard-url
                    ;; 通过 url-retrieve-synchronously 获取 URL 页面内容，
                    ;; 解析 HTML 中的 <title> 标签作为链接描述
                    (read-string "title: "
                                 (with-current-buffer (url-retrieve-synchronously clipboard-url)
                                   (dom-text (car
                                              (dom-by-tag (libxml-parse-html-region
                                                           (point-min)
                                                           (point-max))
                                                          'title))))))))
          (t
           ;; 情形3：其他情况 => 调用标准交互式链接插入
           (call-interactively 'org-insert-link)))))

;; https://emacs-china.org/t/org-mode-narrow-to-sublist/24682/5
(defun org-narrow-to-item ()
  "将缓冲区窄化为当前列表项的范围。

该函数使用 Org 列表导航函数定位当前列表项的起始和结束位置，
然后使用 narrow-to-region 将缓冲区显示范围限制在该项内。
若不在列表中则会发生错误。"
  (interactive)
  (save-excursion
    ;; 定位到当前列表项的起始位置
    (narrow-to-region
     (progn (org-beginning-of-item) (point))  ;; 列表项起始
     (progn (org-end-of-item) (1- (point))))))



(defun douo/inside-writing ()
  "判断当前缓冲区对应的文件是否位于写作目录（douo/writing-home）内。

该函数用于确定是否应应用某些特定于写作目录的功能（如搜索时包含隐藏文件）。
返回值：如果缓冲区有关联的文件路径且该路径以 douo/writing-home 开头则返回 t，否则返回 nil。"
  (let ((file-path (buffer-file-name)))
    ;; 检查缓冲区是否有关联文件，以及该文件路径是否在写作目录内
    (and file-path
         (string-prefix-p douo/writing-home file-path))))

(defun douo/writing-consult-ripgrep-include-hidden-advice (orig-fun &rest args)
  "为 `consult-ripgrep' 添加 :around 建议，在特定写作目录中自动包含隐藏文件.

  当 douo/inside-writing 函数返回 t 时，此建议 (advice) 会
  临时向 `consult-ripgrep-args' 变量追加 --hidden 参数。
  这主要用于确保 ripgrep 能搜索到隐藏的归档目录，如 .archive。

  ORIG-FUN 是被建议的原始函数 (consult-ripgrep)。
  ARGS 是传递给原始函数的参数列表。

  函数通过 `unwind-protect' 确保 `consult-ripgrep-args' 的值在
  搜索结束后总能被恢复，即使发生错误。"
  (let ((original consult-ripgrep-args))
    (if (douo/inside-writing)
        (setq consult-ripgrep-args
              (concat consult-ripgrep-args " --hidden")))
    (unwind-protect
        (apply orig-fun args) ; Execute the original function
      (setq consult-ripgrep-args original))  ; Restore the original value
    )
  )

(advice-add 'consult-ripgrep :around #'douo/writing-consult-ripgrep-include-hidden-advice)

;; config
(use-package org
  :straight (:type built-in)
  :config
  ;; 让 emphasis 块在紧邻中文字符时也能生效
  (org-set-emph-re 'org-emphasis-regexp-components
                   (let ((cjk "[:nonascii:]")) ;; 应该使用 \\cc\\cj\\ch 但 char alternates 不支持 category 所以只能用 char class.
                     (pcase-let ((`(,f ,s . ,r) org-emphasis-regexp-components))
                       `(,(concat f cjk) ,(concat s cjk) . ,r)
                       )
                     ))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)))
  ;; begin_vertico
  ;; 见 https://github.com/minad/vertico#org-agenda-filter-and-org-tags-view
  (defun org-enforce-basic-completion (&rest args)
    "为 Org 标签匹配和过滤操作强制使用基础补全样式，确保 Vertico 兼容性。

该函数作为 :around advice 被应用到 org-make-tags-matcher 和 org-agenda-filter，
它在这些命令中临时禁用高级补全样式（如 flex、partial-completion），
只保留 'basic 补全样式。同时配置 TAB 键为 minibuffer-complete，
预设选择项为 'prompt（即编辑区）而不是第一个候选项。
这确保了 Vertico 补全框架与 Org 标签操作的正确交互。

ARGS 是传递给原始函数的参数列表。"
    (minibuffer-with-setup-hook
        (:append
         (lambda ()
           ;; 创建本地 keymap 并将 TAB 键绑定到基础补全函数
           (let ((map (make-sparse-keymap (current-local-map))))
             (define-key map [tab] #'minibuffer-complete)
             (use-local-map map))
           ;; 强制补全样式为 basic，禁用 flex 等高级样式；设置 vertico 选项
           (setq-local completion-styles (cons 'basic completion-styles)
                       vertico-preselect 'prompt)))
      (apply args)))
  (advice-add #'org-make-tags-matcher :around #'org-enforce-basic-completion)
  (advice-add #'org-agenda-filter :around #'org-enforce-basic-completion)
  ;; end_vertico
  ;; Patch: 统一 org-id（及 org-attach 等模块）ID（UUID）为小写，跨平台性能稳定
  ;;
  ;; 根本原因说明（参考 emacs.stackexchange.com/questions/69236）
  ;; ------------------------------------------------------------
  ;; org-id 和 org-attach 默认调用 uuidgen 工具或相关 Emacs 内部逻辑生成唯一 ID。
  ;; macOS上的uuidgen工具默认输出大写UUID，Linux/部分BSD为小写。
  ;; Org-mode直接用UUID作为ID属性和目录名。
  ;; 由于不同操作系统和同步工具对大小写敏感（如Syncthing以及大部分Linux/macOS文件系统），
  ;; 大小写不一致时会导致 org-attach 目录路径找不到、附件链接丢失及同步冲突。
  ;; 本 patch 利用 advice 机制强制 org-id-new 函数返回小写ID，从根源上统一 ID
  ;; 格式，彻底杜绝因平台差异引发的路径冲突、找不到附件、同步失败和数据混乱问题。
  ;;
  ;; 相关讨论与原因细节见:
  ;; https://emacs.stackexchange.com/questions/69236/how-to-force-org-attach-to-create-lowercase-uuids
  ;;
  ;; 历史数据，不导致冲突暂时不做处理
  ;; ------------------------------------------------------------

  (advice-add 'org-id-new :filter-return #'downcase)
  ;; 如果 emacs 不支持 sound， org-timer 使用外部程序播放提示音
  (unless (fboundp 'play-sound-internal)
    (add-hook 'org-timer-done-hook
              (lambda ()
                (start-process-shell-command "org-timer" nil
                                             (concat (cond
                                                      ((eq system-type 'darwin) "afplay ")
                                                      ((eq system-type 'gnu/linux) "paplay ")
                                                      )
                                                     org-clock-sound)))))
  ;; 增加 latex preview 尺寸
  ;; arch 需安装 texlive 组
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
  :custom
  (org-M-RET-may-split-line
   '((item . nil)     ;; 插入 item 时不 split
     (default . t))) ;; 其它场景保持默认
  (org-directory douo/writing-home)

  ;; (org-preview-latex-default-process 'dvisvgm)
  ;; 设置 timer 的提示音
  (org-clock-sound  (concat (file-name-directory user-init-file) "assets/org-timer.wav"))

  ;; begin_refile
  (org-refile-targets `(
                        (,(concat (file-name-as-directory douo/gtd-home) "tasks.org") :maxlevel . 3)
                        ))
  ;; 显示 refile 的 outline 层级
  ;; 设置为 `file' 会显示文件名，对于我的 gtd 系统来说不是很有用
  (org-refile-use-outline-path 't)
  ;; refile 直接显示目标，而不是通过 outline 层级一层层进入(默认，与 vertico 不兼容)
  (org-outline-path-complete-in-steps nil)
  ;; verico 要使用 outline-path-complete-in-steps 见 https://github.com/minad/vertico#org-refile
  ;; end_refile
  ;; begin_export
  ;; xelatex 较新，支持 unicode 编码，配合宏包，测试过的宏包
  ;; - #+LATEX_HEADER: \usepackage{xeCJK}
  ;; - #+LATEX_HEADER: \usepackage{ctex}
  ;; pdflatex 较旧，要支持中文需使用宏包 CJK
  (org-latex-compiler "xelatex")  ;; 为单文件设置 #+LaTeX_COMPILER: xelatex
  ;; end_export
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         :map org-mode-map
         ("C-c C-l" . ar/org-insert-link-dwim)
         )
  :hook
  (org-mode . org-add-electric-pairs))

;;优先度可以继承
;; https://emacs.stackexchange.com/questions/37800/how-to-inherit-priority-in-org-mode
(defun douo/org-inherited-priority (s)
  "计算一个 Org 标题条目的优先级值，支持从父标题继承优先级。

该函数递归地检查当前标题是否有优先级 cookie（如 [#A]）。
若有则返回其优先级值；若无则检查是否已到达树的根部；
最后若都无则递归上升一级并检查父标题的优先级。
返回值用于 Org agenda 的排序，乘以 1000 以确保正确的线性排序。

S 是当前标题的内容字符串，通常由 org-get-heading 获得。"
  (cond
   ;; 情形1：当前标题有优先级 cookie（如 [#A]、[#B] 等）
   ((string-match org-priority-regexp s)
    ;; 优先级值 = (最低值 - 该条目优先级值) * 1000
    (* 1000 (- org-priority-lowest
               (org-priority-to-value (match-string 2 s)))))

   ;; 情形2：当前标题无优先级 cookie，且已到达 Org 树顶部（无上级标题）
   ((not (org-up-heading-safe))
    ;; 使用默认优先级
    (* 1000 (- org-priority-lowest org-priority-default)))

   ;; 情形3：当前标题无优先级 cookie，继续向上递归检查父标题
   (t
    ;; 递归调用，获取父标题的优先级
    (douo/org-inherited-priority (org-get-heading)))))

;; 解决 org-mode table 中英文混排对齐问题
;; 无需设置中西文等宽字体（https://feeshy.github.io/lists/monospace-fonts-width）
;; XXX valign 会导致 emacs hangs: https://github.com/casouri/valign/issues/35
;; (use-package valign
;;   :straight t
;;   :hook
;;   (org-mode . valign-mode))

(setq org-priority-get-priority-function #'douo/org-inherited-priority)

(use-package org-gtd
  :straight t
  :after org
  :demand t
  :init
  ;; 已完成 4.0 配置迁移后，显式确认升级提示。
  (setq org-gtd-update-ack "4.0.0")
  :config
  ;; org-gtd 4.x 默认把主任务文件名固定为 org-gtd-tasks.org。
  ;; 这里统一覆盖为 tasks.org，确保自动 refile、target 创建和默认文件访问都落到旧文件名。
  (setq org-gtd-default-file-name "tasks")

  (defun douo/org-gtd-set-tags-from-source-context ()
    "在 org-gtd 组织阶段借用源条目文件的 tags 上下文执行 `org-set-tags-command'。

这是 issue #145 讨论里更简单的做法：不去同步整个 clarify buffer 的环境，
而是在真正需要选择 tags 的那一刻，从源条目所在 buffer 读取其 `#+TAGS:'
解析结果，临时 let-bind 给当前 buffer，然后直接调用原生
`org-set-tags-command'。这样可以复用原有 UI，同时避免额外 advice 和临时
buffer 篡改。"
    (interactive)
    (let* ((source-marker (and (boundp 'org-gtd-clarify--source-heading-marker)
                               org-gtd-clarify--source-heading-marker))
           (source-buffer (and (markerp source-marker)
                               (buffer-live-p (marker-buffer source-marker))
                               (marker-buffer source-marker))))
      (if source-buffer
          (let ((source-tag-context
                 (with-current-buffer source-buffer
                   (org-set-regexps-and-options 'tags-only)
                   (list :org-tag-alist org-tag-alist
                         :org-current-tag-alist org-current-tag-alist
                         :org-tag-persistent-alist org-tag-persistent-alist))))
            (let ((org-tag-alist (plist-get source-tag-context :org-tag-alist))
                  (org-current-tag-alist
                   (plist-get source-tag-context :org-current-tag-alist))
                  (org-tag-persistent-alist
                   (plist-get source-tag-context :org-tag-persistent-alist)))
              (call-interactively #'org-set-tags-command)))
        (call-interactively #'org-set-tags-command))))

  ;; lambda 值不适合 customize-set-variable，在 :config 里用 setq 设置归档路径。
  ;; 自定义归档路径为 .archive/gtd_{year}.org
  (setq org-gtd-archive-location
        (lambda ()
          (let ((year (number-to-string (caddr (calendar-current-date)))))
            (string-join `(,douo/gtd-home "/.archive/gtd_" ,year ".org::datetree/")))))

  ;; 启用全局 GTD mode-line 指示器，在 mode-line 显示收件箱待处理数量（如 GTD[5]）。
  (org-gtd-mode 1)

  ;; modify org capture templates
  (nconc
   org-gtd-capture-templates
   `(
     ;; Quick Note
     ("n" "Quick Note"
      plain (file (lambda () (douo/generate-quick-note (concat douo/writing-home "/_notes/Quick"))))
      "%i\n%U\n%?\n"
      :kill-buffer t)
     ;; 用于 Chrome 通过 org-protocol 捕获当前链接到 inbox
     ("r"
      "Store a link to read later"
      entry (file ,#'org-gtd-inbox-path)
      ,(format "* %s %%a %%(org-set-tags \"read\")\n%%i\n%%U\n%%?"
           (alist-get 'todo org-gtd-keyword-mapping))
      :empty-lines 1
      :kill-buffer t
      )
     )
   )
  :custom
    ;; GTD 关键字单独放在一条 sequence 里，后续 writing 系统可再追加自己的 sequence。
    (org-todo-keywords
     '((sequence "TODO" "NEXT" "WAIT" "|" "DONE" "CNCL")))
    ;; 4.x 使用统一 mapping，把 GTD 语义状态映射到全局 TODO 关键字。
    (org-gtd-keyword-mapping
     '((todo . "TODO")
       (next . "NEXT")
       (wait . "WAIT")
       (done . "DONE")
       (canceled . "CNCL")))
  (org-gtd-directory douo/gtd-home)
  ;; 收件箱有待处理条目时才显示 mode-line 指示器，避免 GTD[0] 的干扰。
  (org-gtd-mode-lighter-display 'when-non-zero)
      ;; 仅在真正需要设置 tags 的时刻，借用源条目文件的 tags 上下文。
      (org-gtd-organize-hooks '(douo/org-gtd-set-tags-from-source-context))

  :bind
  (("C-c c" . org-gtd-capture)
   ("C-c d e" . douo/org-gtd-engage)
   ("C-c d p" . org-gtd-process-inbox)
   ("C-c d n" . org-gtd-show-all-next)
   ("C-c d s" . org-gtd-reflect-stuck-projects)
   :map org-gtd-clarify-mode-map
   ("C-c C" . org-gtd-organize)
   :map org-mode-map
   ("C-c d a" . douo/org-gtd-archive)))

;; this allows you use `(,org-gtd-directory) for your agenda files
(use-package org-agenda
  :straight nil
  :after org-gtd
  :custom
  (org-agenda-files `(,org-gtd-directory))
  :bind
  (:map org-agenda-mode-map
        ("t" . douo/org-agenda-todo-dwim)
        ("T" . org-agenda-todo)
        ("C-o" . casual-agenda-tmenu))
  :defer t)
(use-package org-contrib ; Includes more than the standard org-mode
  :straight  '(org-contrib :includes org-protocol)
  :after org-gtd
  :config
  ;; Your other org-mode configurations here
  (require 'org-tempo) ;; <s TAB 补全
  (require 'org-protocol)
  (defun org-gtd-protocol-capture (info)
    "通过 Org-protocol 外部协议从浏览器等外部应用捕获任务。

该函数作为自定义 Org-protocol 处理器注册，当用户通过
诸如 \"org-protocol://gtd-capture?url=...&title=...\" 这样的 URL
触发时被调用。它会解析 INFO 参数中的 URL、标题等信息，
然后在 Org-GTD 的 inbox 中创建新的 TODO 条目。

这通常与浏览器插件（如 Org-web-clipper）配合使用，
实现从网页快速收集信息到 GTD 系统的工作流。

INFO 是 org-protocol 解析的参数字典。"
    ;; 打印接收到的 URL 和其他参数信息，便于调试
    (message "Org-protocol INFO received: %s" info)
    ;; 在 Org-GTD 捕获上下文中调用标准 org-protocol-capture 处理器
    (with-org-gtd-capture
     (org-protocol-capture info))
    )
  ;; 自定义 org-protocol 的 gtd-capture 模板
  (setq org-protocol-protocol-alist '(("org-gtd-capture"  :protocol "gtd-capture"   :function org-gtd-protocol-capture))))



(use-package org-agenda-property
  :straight t
  )

(use-package org-edna
  :straight t
  :config
  (setq org-edna-use-inheritance t)
  :hook
  (org-mode . org-edna-mode)
  )

;; https://github.com/minad/org-modern
;; 美化似乎只是一时的新鲜感
;; (use-package org-modern
;;   :straight t
;;   :hook
;;   (org-mode . org-modern-mode)
;;   (org-agenda-finalize-hook . org-modern-mode)
;;   )


;; https://orgmode.org/worg/org-contrib/org-checklist.html
;; 添加属性 RESET_CHECK_BOXES
;; 切换 TODO 状态时，重置所有 checkbox 的状态
(use-package org-checklist
  :straight  '(org-contrib :includes org-checklist)
  )


(use-package org-roam
  :straight t
  :init
  (defun org-roam-note-find (arg)
    "打开 Org-roam 笔记节点选择器，但后台过滤掉 GTD 特定目录的节点。

该函数为 org-roam-node-find 的自定义包装，通过在 org-roam-node-find
调用时传入自定义的过滤函数（filter），来排除 _gtd 目录及其子目录中
的笔记节点。这样可以保持 Roam 图谱的核心知识库与 GTD 系统的分离。

当带负参数 (C-u) 调用时将在其他窗口打开节点；无参数时在当前窗口打开。

ARG：前缀参数，非 nil 时表示在其他窗口打开笔记。"
    (interactive "P")
    ;; 根据前缀参数决定是否在其他窗口打开笔记
    (let ((other-window (if arg t nil)))
      ;; 调用 org-roam-node-find，并传入过滤函数排除 _gtd 目录内的节点
      (org-roam-node-find other-window nil (lambda (node)
                                             ;; 过滤器返回 t 表示保留该节点，nil 表示排除
                                             (not (string-match-p "/_gtd/" (org-roam-node-file node)))
                                             ))))
  :custom
  (org-roam-directory (file-truename (concat douo/writing-home "/_roam/")))
  ;; format org-roam-node to file-title/title if level larger than 0
  ;; 对于 header 类的 node 显示其所在文件 node 的标题
  ;; 该变量用于 org-roam-node-insert 所插入链接的 description 部分
  ;; 被 `org-roam-node-formatted' 所调用
  ;; TODO `org-roam-complete-link-at-point' 是写死使用的 `org-roam--get-titles'，需要修改
  (org-roam-node-formatter (lambda (node)
                             (let ((level (org-roam-node-level node)))
                               (if (> level 0)
                                   (format "%s->%s" (org-roam-node-file-title node) (org-roam-node-title node))
                                 (org-roam-node-title node)))))
  (org-roam-dailies-directory "quick/")
  :bind-keymap ("C-c n" . my-org-roam-map)
  :bind (:map my-org-roam-map
              ("l" . org-roam-buffer-toggle)
              ("f" . org-roam-note-find)
              ("g" . org-roam-graph)
              ("i" . org-roam-node-insert)
              ("c" . org-roam-capture)
              ;; Dailies
              ("j" . org-roam-dailies-capture-today))
  :config
  (define-prefix-command 'my-org-roam-map)
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${formatted:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  )

(use-package consult-org-roam
  ;; 通过 `consult-org-roam-node-read' 为选择 node 提供 consult 包装（主要支持 preview ）
  :straight t
  :after org-roam
  :custom
  ;; Use `ripgrep' for searching with `consult-org-roam-search'
  (consult-org-roam-grep-func #'consult-ripgrep)
  ;; 用于在 consult-buffer 中过滤出 roam node
  (consult-org-roam-buffer-narrow-key ?r)
  ;; 禁用 consult-org-roam-buffer , 困扰多过实用
  (consult-org-roam-buffer-enabled nil)
  ;; Display org-roam buffers right after non-org-roam buffers
  ;; in consult-buffer (and not down at the bottom)
  (consult-org-roam-buffer-after-buffers nil)
  :config
  ;; Eventually suppress previewing for certain functions
  (consult-customize
   consult-org-roam-forward-links
   :preview-key (kbd "M-."))
  (consult-org-roam-mode 1)
  :bind
  (:map my-org-roam-map
  ("F" . consult-org-roam-file-find) ;; 只列出文件
  ("b" . consult-org-roam-backlinks)
  ("k" . consult-org-roam-forward-links)
  ;; TODO 可以整合 `deft' 的功能
  ("r" . consult-org-roam-search)))



(use-package org-roam-ui
  :straight t
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t)
  :commands (org-roam-ui-mode))


;; - 在 Org 笔记文件的一个标题内 M-x org-noter
;;   这会将该标题与一个文档关联起来，并用它打开一个会话。
;; - 文档中 M-x org-noter
;;   在浏览文档时（例如 PDF、epub……）运行 M-x org-noter。
;;   这会尝试自动查找相应的笔记文件。它会在所有父文件夹和你设置的某些特定文件夹中搜索。
(use-package org-noter
  :straight t
  :commands (org-noter))

;; 导出
(use-package htmlize
  :straight t
  :after org
  :commands (htmlize-buffer htmlize-file htmlize-many-files))


;; org 当前元素相关的 transient 菜单
;; TODO 相当于一个简短的提醒菜单，现在功能还不是很好，以后可以自己整理
;; https://github.com/alphapapa/org-ql?tab=readme-ov-file#queries
(use-package org-menu
  :straight t
  :after org
  :bind
  (:map org-mode-map
        ;; 覆盖了 org-open-line
        ("C-o" . org-menu)))

;; 与 org-menu 职责有重叠，但是 embark 根据当前元素的上下文提供更精细的操作
(use-package embark-org
  :after (embark org)
  :config
  (defun douo/embark-org-timestamp-target ()
    "判断当前位置是不是 timestamp ，返回 `embark-org-target-element-context' 相同的结构，
但是不同的是，它无视上下文，只要满足时间戳的正则就返回"
    (when (org-at-timestamp-p 'lax) ;; 无视上下文
      (let* ((begin (match-beginning 0))
             (end (match-end 0))
             (ts (buffer-substring-no-properties begin end)))
        `(org-timestamp ,ts ,begin . ,end))))
  ;; 添加在 `embark-org-target-element-context' 之后
  (if-let* (((not (memq 'douo/embark-org-timestamp-target embark-target-finders)))
           (tail (memq 'embark-org-target-element-context embark-target-finders)))
      (push 'douo/embark-org-timestamp-target (cdr tail))
    (push 'douo/embark-org-timestamp-target embark-target-finders))
  ;; 添加 embark-org-timestamp-map
  (add-to-list 'embark-keymap-alist '(org-timestamp . embark-org-timestamp-map))
  (defvar-keymap embark-org-timestamp-map
    :doc "Keymap for actions on org-timestamps."
    :parent embark-general-map
    "RET" #'org-menu-fix-timestamp ; harmless default
    "t" #'org-toggle-timestamp-type
    "e" (lambda (_) "edit a time stamp." nil (org-time-stamp nil))
    )
  ;; modify default embark key for org-mode
  (defun douo/setup-embark-org-keymap ()
    "为 org-mode 的 region 目标补充时间戳动作键映射。"
    (add-to-list 'embark-keymap-alist '(region . embark-org-timestamp-map))
    ))

;; org-ql
;; 该包提供了 Org 文件的查询语言。它提供两种语法风格：类似 Lisp 的 sexps 和类似搜索引擎的关键字。
;; ex: 7 月份完成的任务
;; (org-ql-search (org-agenda-files) "todo:DONE ts:from=2024-07-01,to=2024-07-31" :narrow nil :super-groups '((:auto-tags)) :sort nil)
(use-package org-ql
  :straight t
  :after org
  :commands (org-ql-find org-ql-search org-ql-view))

(use-package org-analyzer
  :straight t
  :after org
  :commands (org-analyzer-start))

(provide 'init-org)

;;; init-org.el ends here
