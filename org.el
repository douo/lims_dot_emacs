(defun douo/generate-quick-note (path)
  (let ((file
         (expand-file-name (format-time-string "%Y/%m/note-%Y-%m-%d.org") path)))
    (if (f-exists? file)
        file
      (mkdir (f-dirname file) t)
      (f-write-text
       (concat "#+TITLE: " (format-time-string "%Y年%m月%d日杂记") "\n"
               "#+date: " (format-time-string "[%Y-%m-%d]") "\n\n")
       'utf-8
       file
       )
      file
      )
    )
  )

(defun douo/insert-header-from-note-name ()
  (interactive)
  (let ((date (encode-time (org-parse-time-string (substring (buffer-name) 5 -4)))))
    (save-excursion
      (goto-char (point-min))
    (insert (concat "#+TITLE: " (org-format-time-string "%Y年%m月%d日杂记" date nil)  "\n"
                 "#+date: " (format-time-string "[%Y-%m-%d]" date) "\n\n")
      )))
  )

;; https://emacs.stackexchange.com/a/2559/30746
;; FYI: 手动补全 C-c C-x C-f *
;; FYI: org-emphasize
(defvar org-electric-pairs '((?* . ?*) (?~ . ?~) (?+ . ?+) (?_ . ?_) (?= . ?=)))
(defun org-add-electric-pairs ()
  "自动补全文本强调（emphasis）语法的分隔符."
  (setq-local
   electric-pair-pairs (append electric-pair-pairs org-electric-pairs))
  (setq-local
   electric-pair-text-pairs electric-pair-pairs)
  )


;; https://xenodium.com/emacs-dwim-do-what-i-mean/
;; 检测剪切板是否是 url
;; 剪切板 url + region 自动插入
;; 自动解析 url 获取 title
;; fallback 默认 org-insert-link
(defun ar/org-insert-link-dwim ()
  "Like `org-insert-link' but with personal dwim preferences."
  (interactive)
  (let* ((point-in-link (org-in-regexp org-link-any-re 1))
         (clipboard-url (when (string-match-p "^http" (current-kill 0))
                          (current-kill 0)))
         (region-content (when (region-active-p)
                           (buffer-substring-no-properties (region-beginning)
                                                           (region-end)))))
    (cond ((and region-content clipboard-url (not point-in-link))
           (delete-region (region-beginning) (region-end))
           (insert (org-make-link-string clipboard-url region-content)))
          ((and clipboard-url (not point-in-link))
           (insert (org-make-link-string
                    clipboard-url
                    (read-string "title: "
                                 (with-current-buffer (url-retrieve-synchronously clipboard-url)
                                   (dom-text (car
                                              (dom-by-tag (libxml-parse-html-region
                                                           (point-min)
                                                           (point-max))
                                                          'title))))))))
          (t
           (call-interactively 'org-insert-link)))))

;; https://emacs-china.org/t/org-mode-narrow-to-sublist/24682/5
(defun org-narrow-to-item ()
  "Narrow buffer to the current item.

Throw an error when not in a list."
  (interactive)
  (save-excursion
    (narrow-to-region
	 (progn (org-beginning-of-item) (point))
	 (progn (org-end-of-item) (1- (point))))))

;; config
(use-package org
  :straight t
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
    (minibuffer-with-setup-hook
        (:append
         (lambda ()
           (let ((map (make-sparse-keymap (current-local-map))))
             (define-key map [tab] #'minibuffer-complete)
             (use-local-map map))
           (setq-local completion-styles (cons 'basic completion-styles)
                       vertico-preselect 'prompt)))
      (apply args)))
  (advice-add #'org-make-tags-matcher :around #'org-enforce-basic-completion)
  (advice-add #'org-agenda-filter :around #'org-enforce-basic-completion)
  ;; end_vertico
  :custom
  (org-directory (if (not (string-suffix-p "/" douo/gtd-home))
      (concat douo/gtd-home "/")
    douo/gtd-home))
  ;; a useful view to see what can be accomplished today
  (org-refile-targets `(
                        (,(concat douo/gtd-home "/tasks.org") :maxlevel . 3)
                        ))
  ;; (org-preview-latex-default-process 'dvisvgm)
  (org-clock-sound  (concat (file-name-directory user-init-file) "org-timer.mp3"))
  ;; begin_refile
  ;; 显示 refile 的 outline 层级
  ;; 设置为 `file' 会显示文件名，对于我的 gtd 系统来说不是很有用
  (org-refile-use-outline-path 't)
  ;; refile 直接显示目标，而不是通过 outline 层级一层层进入(默认，与 vertico 不兼容)
  (org-outline-path-complete-in-steps nil)
  ;; verico 要使用 outline-path-complete-in-steps 见 https://github.com/minad/vertico#org-refile
  ;; end_refile
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         :map org-mode-map
         ("C-c C-l" . ar/org-insert-link-dwim)
         )
  :hook
  (org-mode . org-add-electric-pairs)
  )


;; 优先度可以继承
;; https://emacs.stackexchange.com/questions/37800/how-to-inherit-priority-in-org-mode
(defun douo/org-inherited-priority (s)
  (cond

   ;; Priority cookie in this heading
   ((string-match org-priority-regexp s)
    (* 1000 (- org-priority-lowest
               (org-priority-to-value (match-string 2 s)))))

   ;; No priority cookie, but already at highest level
   ((not (org-up-heading-safe))
    (* 1000 (- org-priority-lowest org-priority-default)))

   ;; Look for the parent's priority
   (t
    (douo/org-inherited-priority (org-get-heading)))))

(setq org-priority-get-priority-function #'douo/org-inherited-priority)

;; this allows you use `(,org-gtd-directory) for your agenda files
(use-package org-agenda
  :straight nil
  :after org-gtd
  :custom
  (org-agenda-files `(,org-gtd-directory))
  (org-agenda-custom-commands '(("g" "Scheduled today and all NEXT items" ((agenda "" ((org-agenda-span 1))) (todo "NEXT")))))
  )

(use-package org-gtd
  :straight t
  :after org
  :demand t
  :init
  (setq org-gtd-update-ack "3.0.0")
  (defun douo/org-gtd-archive ()
    "Process GTD inbox item as a reference item without jump to inbox."
    (interactive)
    (with-org-gtd-context (org-archive-subtree))
    )
  :config
  (defun douo/org-gtd-engage()
    "Display `org-agenda' customized by org-gtd."
    (interactive)
    (org-gtd-core-prepare-agenda-buffers)
    (with-org-gtd-context
        (let ((org-agenda-custom-commands
  `(("g" "Scheduled today and all NEXT items"
     ((agenda "" ((org-agenda-span 1)
                  (org-agenda-start-day nil)
                  (org-agenda-skip-additional-timestamps-same-entry t)))
      (todo org-gtd-next
            ((org-agenda-overriding-header "All NEXT items")
             (org-agenda-prefix-format
              '((todo . " %i %-12:(concat \"[\"(org-format-outline-path (org-get-outline-path)) \"] \")")))))
      (todo org-gtd-wait
            ((org-agenda-todo-ignore-with-date t)
             (org-agenda-overriding-header "Delegated/Blocked items")
             (org-agenda-prefix-format
              '((todo . " %i %-12 (org-gtd-agenda--prefix-format)"))))))))))
          (org-agenda nil "g")
          (goto-char (point-min)))))
  ;; Temporary fix see #https://github.com/Trevoke/org-gtd.el/pull/151
  (defun org-gtd-clarify-item ()
  "Process item at point through org-gtd."
  (declare (modes org-mode)) ;; for 27.2 compatibility
  (interactive)
  (let ((processing-buffer (org-gtd-clarify--get-buffer))
        (window-config (current-window-configuration))
        (source-heading-marker (point-marker))
        (inbox-current-tags-alist org-current-tag-alist)
        )
    (org-gtd-clarify--maybe-initialize-buffer-contents processing-buffer)
    (with-current-buffer processing-buffer
      (setq-local org-gtd-clarify--window-config window-config
                  org-gtd-clarify--source-heading-marker source-heading-marker
                  org-gtd-clarify--clarify-id (org-id-get)
                  org-current-tag-alist inbox-current-tags-alist))
    (org-gtd-clarify-setup-windows processing-buffer)))
  ;;  modify org capture templates
  (add-to-list
   'org-gtd-capture-templates
   '("n" "Quick Note"
     plain (file (lambda () (douo/generate-quick-note (concat douo/writing-home "/_notes/Quick"))))
     "%i\n%U\n%?\n"
     :kill-buffer t)
   )
  :custom
  (org-gtd-directory org-directory)
  ;; 自定义归档路径为 .archive/gtd_{2023}.org
  (org-gtd-archive-location (lambda ()
    (let ((year (number-to-string (caddr (calendar-current-date)))))
      (string-join `(".archive/gtd_" ,year  ".org::datetree/")))))
  ;; 让 todo 显示所有 outline path

  :bind
  (("C-c c" . org-gtd-capture)
   ("C-c d e" . douo/org-gtd-engage)
   ("C-c d p" . org-gtd-process-inbox)
   ("C-c d n" . org-gtd-show-all-next)
   ("C-c d s" . org-gtd-show-stuck-projects)
   :map org-gtd-clarify-map
   ("C-c C" . org-gtd-organize)
   :map org-mode-map
   ("C-c d a" . douo/org-gtd-archive)
   )
  )

;;
;;
(use-package org-analyzer
  :straight t
  :after org)


;; this allows you to use (org-gtd-inbox-path) for your capture destinations
(use-package org-capture
  :straight nil
  :after org-gtd
  )

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

(use-package org-download
  :straight t
  :after org
  :custom
  (org-download-method 'attach)
  :bind
  (:map org-mode-map
        ("C-c C-x p" . org-download-clipboard))
  )

;; https://github.com/minad/org-modern
;; 美好似乎只是一时的新鲜感
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
  :custom
  (org-roam-directory (file-truename (concat douo/writing-home "/_roam/")))
  (org-roam-dailies-directory "quick/")
  :bind
  ("C-c n l" . org-roam-buffer-toggle)
    ;; ("C-c n f" . org-roam-node-find) ;; 由 `consult-org-roam-file-find'  代替
  ("C-c n g" . org-roam-graph)
  ("C-c n i" . org-roam-node-insert)
  ("C-c n c" . org-roam-capture)
  ;; Dailies
  ("C-c n j" . org-roam-dailies-capture-today)
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  )
(require 'org-roam-protocol)


(use-package consult-org-roam
   :straight t
   :after org-roam
   :init
   (require 'consult-org-roam)
   ;; Activate the minor mode
   (consult-org-roam-mode 1)
   :custom
   ;; Use `ripgrep' for searching with `consult-org-roam-search'
   (consult-org-roam-grep-func #'consult-ripgrep)
   ;; Configure a custom narrow key for `consult-buffer'
   (consult-org-roam-buffer-narrow-key ?r)
   ;; Display org-roam buffers right after non-org-roam buffers
   ;; in consult-buffer (and not down at the bottom)
   (consult-org-roam-buffer-after-buffers t)
   :config
   ;; Eventually suppress previewing for certain functions
   (consult-customize
    consult-org-roam-forward-links
    :preview-key (kbd "M-."))
   :bind
   ;; Define some convenient keybindings as an addition
   ("C-c n f" . consult-org-roam-file-find)
   ("C-c n b" . consult-org-roam-backlinks)
   ("C-c n l" . consult-org-roam-forward-links)
   ("C-c n r" . consult-org-roam-search) ;; TODO 可以整合 `deft' 的功能
   )


(use-package org-noter
  :straight t)
