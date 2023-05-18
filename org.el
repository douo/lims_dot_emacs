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

;; config
(use-package org
  :ensure t
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
  :custom

  (org-directory (if (not (string-suffix-p "/" douo/gtd-home))
      (concat douo/gtd-home "/")
    douo/gtd-home))
  ;; a useful view to see what can be accomplished today
  (org-refile-targets `(
                        (,(concat douo/gtd-home "/tasks.org") :maxlevel . 3)
                        ))
  (org-preview-latex-default-process 'dvisvgm)
  (org-clock-sound  (concat (file-name-directory user-init-file) "org-timer.mp3"))
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
  :ensure nil
  :after org-gtd
  :custom
  (org-agenda-files `(,org-gtd-directory))
  (org-agenda-custom-commands '(("g" "Scheduled today and all NEXT items" ((agenda "" ((org-agenda-span 1))) (todo "NEXT")))))
  )

(use-package org-gtd
  :ensure t
  :after org
  :demand t
  :init
  (defun douo/org-gtd-archive ()
    "Process GTD inbox item as a reference item without jump to inbox."
    (interactive)
    (with-org-gtd-context (org-archive-subtree))
    )
  :custom
  (org-gtd-update-ack "3.0.0")
  (org-gtd-directory douo/gtd-home)
  ;; 自定义归档路径为 .archive/gtd_{2023}.org
  (org-gtd-archive-location (lambda ()
    (let ((year (number-to-string (caddr (calendar-current-date)))))
      (string-join `(".archive/gtd_" ,year  ".org::datetree/")))))
  ;; 让 todo 显示所有 outline path
  (org-gtd-agenda-custom-commands
  '(("g" "Scheduled today and all NEXT items"
     (
      (agenda "" ((org-agenda-span 1)
                  (org-agenda-start-day nil)))
      (todo "NEXT" ((org-agenda-overriding-header "All NEXT items")
                    (org-agenda-prefix-format '((todo . " %i %-12:(concat \"[\"(org-format-outline-path (org-get-outline-path)) \"] \")")))))
      (todo "WAIT" ((org-agenda-todo-ignore-with-date t)
                    (org-agenda-overriding-header "Delegated/Blocked items")
                    (org-agenda-prefix-format '((todo . " %i %-12 (org-gtd--agenda-prefix-format)")))))))))
  :bind
  (("C-c c" . org-gtd-capture)
   ("C-c d e" . org-gtd-engage)
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
  :ensure t
  :after org)


;; this allows you to use (org-gtd-inbox-path) for your capture destinations
(use-package org-capture
  :ensure nil
  :after org-gtd
  :custom
  (org-capture-templates
        '(
          ("n" "Quick Note"
           plain (file (lambda () (douo/generate-quick-note (concat douo/writing-home "/_notes/Quick"))))
           "%i\n%U\n%?\n"
           :kill-buffer t)
          )
        )
  )

(use-package org-agenda-property
  :ensure t
  )

(use-package org-edna
  :ensure t
  :config
  (setq org-edna-use-inheritance t)
  (org-edna-mode 1)
  )

(use-package org-download
  :ensure t
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
;;   :ensure t
;;   :hook
;;   (org-mode . org-modern-mode)
;;   (org-agenda-finalize-hook . org-modern-mode)
;;   )
