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
  "使用 org-tasklet 将当前条目归档到年度归档文件。"
  (interactive)
  ;; 兼容旧函数名，避免其他个人快捷键或命令历史失效。
  (org-tasklet-archive-subtree))

(defun douo/org-gtd-engage ()
  "显示 org-tasklet 总览 agenda。"
  (interactive)
  ;; 兼容旧函数名；新的总览不再包含 WAIT，因为该状态已从工作流移除。
  (org-tasklet-engage))

(defun douo/org-agenda-todo-dwim (&optional arg)
  "在 Agenda 中以 toggle 方式切换 org-tasklet 条目的 TODO 状态。

默认情况下，如果当前 agenda 条目的 TODO 状态属于开放态，
则直接切换到 DONE；如果当前已经是 DONE 或 CNCL，则切回 TODO。

带前缀参数 ARG 时，回退到原生 `org-agenda-todo'，保留循环或手动选择
TODO 状态的能力。"
  (interactive "P")
  (org-tasklet-agenda-todo-dwim arg))

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
  ;; 调整 latex preview 尺寸以匹配编辑器字体大小 (xelatex 矢量图推荐 0.8 - 1.0)
  ;; (setq org-format-latex-options (plist-put org-format-latex-options :scale 0.9))
  ;; 使用 xelatex 渲染高清矢量公式图片 (完美支持 fontspec / 中文等)
  (setq org-preview-latex-default-process 'xelatex)
  ;; 让 LaTeX 预览图片使用无衬线字体 (Sans-Serif)，与编辑器文本字体风格保持协调一致
  (unless (string-match-p "sansmath" org-format-latex-header)
    (setq org-format-latex-header
          (concat org-format-latex-header
                  "\n\\renewcommand{\\familydefault}{\\sfdefault}\n\\usepackage{sansmath}\n\\sansmath\n")))
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

(use-package org-tasklet
  :load-path "~/playground/org-tasklet"
  :after org
  :demand t
  :custom
  ;; 先复用原来的任务目录，方便直接测试旧 inbox.org 和 tasks.org。
  (org-tasklet-directory douo/gtd-home)
  ;; 归档文件名沿用旧习惯，避免新旧归档混在两套命名里。
  (org-tasklet-archive-file-format "gtd_%Y.org")
  ;; 收件箱有待处理条目时才显示 mode-line 指示器。
  (org-tasklet-mode-line-display 'when-non-zero)
  ;; 保留旧浏览器脚本使用的 gtd-capture 协议。
  (org-tasklet-legacy-protocols '("gtd-capture"))
  :config
  ;; 仅个人配置兼容旧 tasks.org 里的拼写 Haits；通用插件默认仍使用 Habits。
  (setf (plist-get (cdr (assq 'habit org-tasklet-organize-types)) :aliases)
        '("Haits"))
  ;; 创建基础文件，加入 agenda，并启用轻量 mode-line 收件箱计数。
  (org-tasklet-setup)
  (org-tasklet-mode 1)
  ;; 显式设置全局键位，避免 `:after org' 场景下 :bind 的注册时机不稳定。
  ;; 常用键位沿用原 org-gtd 入口；C-c d p 按分类整理当前 inbox item，不进入线性流程。
  (global-set-key (kbd "C-c c") #'org-tasklet-capture)
  (global-set-key (kbd "C-c d e") #'org-tasklet-engage)
  (global-set-key (kbd "C-c d p") #'org-tasklet-triage-current-item)
  (global-set-key (kbd "C-c d h") #'org-tasklet-help)
  (global-set-key (kbd "C-c d n") #'org-tasklet-show-next)
  (global-set-key (kbd "C-c d s") #'org-tasklet-reflect-stuck-projects)
  ;; 保留原来的 Quick Note 捕获入口 n。
  (with-eval-after-load 'org-tasklet-capture
    (add-to-list
     'org-tasklet-capture-templates
     `("n" "Quick Note"
       plain (file (lambda () (douo/generate-quick-note (concat douo/writing-home "/_notes/Quick"))))
       "%i\n%U\n%?\n"
       :kill-buffer t)
     t))
  :bind
  (:map org-mode-map
        ;; 在 Org buffer 中也显式绑定，确保编辑 inbox.org 时可以直接处理当前 item。
        ("C-c d e" . org-tasklet-engage)
        ("C-c d p" . org-tasklet-triage-current-item)
        ("C-c d h" . org-tasklet-help)
        ("C-c d n" . org-tasklet-show-next)
        ("C-c d s" . org-tasklet-reflect-stuck-projects)
        ("C-c d a" . org-tasklet-archive-subtree)))

;; 让 org-tasklet 的任务目录参与 agenda。
(use-package org-agenda
  :straight nil
  :after org-tasklet
  :custom
  (org-agenda-files `(,org-tasklet-directory))
  :bind
  (:map org-agenda-mode-map
        ("t" . douo/org-agenda-todo-dwim)
        ("T" . org-agenda-todo)
        ("C-o" . casual-agenda-tmenu))
  :defer t)
(use-package org-contrib ; Includes more than the standard org-mode
  :straight  '(org-contrib :includes org-protocol)
  :after org-tasklet
  :config
  (require 'org-tempo) ;; <s TAB 补全
  ;; 浏览器 org-protocol 需要在 Emacs 服务进程中注册处理器。
  (require 'org-protocol)
  (org-tasklet-register-org-protocol))



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
