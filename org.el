(defun douo/generate-quick-note (path)
  (let ((file
         (expand-file-name (format-time-string "%Y/%m/note-%Y-%m-%d.org") path)))
    (if (f-exists? file)
        file
      (progn
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
  :custom
  (org-directory douo/gtd-home)
  ;; a useful view to see what can be accomplished today
  (org-refile-targets `(
                        (,(concat douo/gtd-home "/tasks.org") :maxlevel . 2)
                        ))
  (org-clock-sound  (concat (file-name-directory user-init-file) "org-timer.mp3"))
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         :map org-mode-map
         ("C-c C-l" . ar/org-insert-link-dwim)
         )
  :hook
  (org-mode . org-add-electric-pairs)
  )

(use-package org-gtd
  :ensure t
  :after org
  :demand t
  :custom
  (org-gtd-directory douo/gtd-home)
  :bind
  (("C-c c" . org-gtd-capture)
   ("C-c d e" . org-gtd-engage)
   ("C-c d p" . org-gtd-process-inbox)
   ("C-c d n" . org-gtd-show-all-next)
   ("C-c d s" . org-gtd-show-stuck-projects)
   :map org-gtd-process-map
   ("C-c C" . org-gtd-choose))
  :config
  )

(use-package org-analyzer
  :ensure t
  :after org)

;; this allows you use `(,org-gtd-directory) for your agenda files
(use-package org-agenda
  :ensure nil
  :after org-gtd
  :custom
  (org-agenda-files `(,org-gtd-directory))
  (org-agenda-custom-commands '(("g" "Scheduled today and all NEXT items" ((agenda "" ((org-agenda-span 1))) (todo "NEXT")))))
  )

;; this allows you to use (org-gtd-inbox-path) for your capture destinations
(use-package org-capture
  :ensure nil
  :after org-gtd
  :custom
  (org-capture-templates
        `(
          ("q" "Quick Note"
           plain (file ,(douo/generate-quick-note (concat douo/writing-home "/_notes/Quick")))
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
