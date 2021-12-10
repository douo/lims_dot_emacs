(defun douo/generate-quick-note (path)
  (expand-file-name (format-time-string "%Y/%m/%Y-%m-%d.org")
                    path))

;; config
(use-package org
  :ensure t
  :config
  ;; a useful view to see what can be accomplished today
  (setq org-agenda-custom-commands '(("g" "Scheduled today and all NEXT items" ((agenda "" ((org-agenda-span 1))) (todo "NEXT")))))
  (setq org-refile-targets `(
                             (,(concat douo/gtd-home "/incubate.org") :maxlevel . 1)
                             (,(concat douo/gtd-home "/actionable.org") :maxlevel . 2)
                             ))

  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         )
  )

(use-package org-gtd
  :ensure t
  :after org
  :demand t
  :config
  (setq org-gtd-directory douo/gtd-home)
  (setq org-agenda-files `(,org-gtd-directory))
  (setq org-capture-templates
        `(
          ("i" "Inbox"
           entry (file ,(org-gtd-inbox-path))
           "* %?\n%U\n\n  %i"
           :kill-buffer t)
          ("l" "Todo with link"
           entry (file ,(org-gtd-inbox-path))
           "* %?\n%U\n\n  %i\n  %a"
           :kill-buffer t)
          ("q" "Quick Note"
           plain (file ,(douo/generate-quick-note (concat douo/writing-home "/_notes/Quick")))
           "%i\n%U\n%?\n"
           )
          )
        )

  (bind-key "C-c C" 'org-gtd-clarify-finalize)
  ;; 重写 incubate，去除强制 schedule
  (defun org-gtd--incubate ()
    "Process GTD inbox item by incubating it.
Allow the user apply user-defined tags from
`org-tag-persistent-alist', `org-tag-alist' or file-local tags in
the inbox.  Refile to `org-gtd-incubate-file-basename'."
    (org-gtd--clarify-item)
    (org-gtd--decorate-item)
    (org-gtd--refile-incubate))

  :bind (("C-c d c" . org-gtd-capture)
         ("C-c d a" . org-agenda-list)
         ("C-c d p" . org-gtd-process-inbox)
         ("C-c d n" . org-gtd-show-all-next)
         ("C-c d s" . org-gtd-show-stuck-projects))
  )

;; this allows you use `(,org-gtd-directory) for your agenda files
(use-package org-agenda
  :ensure nil
  :after org-gtd)

;; this allows you to use (org-gtd-inbox-path) for your capture destinations
(use-package org-capture
  :ensure nil
  :after org-gtd)

(use-package org-agenda-property
  :ensure t
  :config
  (setq org-agenda-property-list '("DELEGATED_TO"))
  )

(use-package org-edna
  :ensure t
  :config
  (setq org-edna-use-inheritance t)
  (org-edna-mode 1)
  )
