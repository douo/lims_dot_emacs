(defun douo/generate-quick-note (path)
  (expand-file-name (format-time-string "%Y/%m/note-%Y-%m-%d.org")
                    path))

;; config
(use-package org
  :ensure t
  :custom
  ;; a useful view to see what can be accomplished today
  (org-refile-targets `(
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
  :custom
  (org-gtd-directory douo/gtd-home)
  :bind
  (("C-c d c" . org-gtd-capture)
   ("C-c d e" . org-gtd-engage)
   ("C-c d p" . org-gtd-process-inbox)
   ("C-c d n" . org-gtd-show-all-next)
   ("C-c d s" . org-gtd-show-stuck-projects)
   :map org-gtd-process-map
   ("C-c C" . org-gtd-choose))
  :config
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
  )

(use-package org-edna
  :ensure t
  :config
  (setq org-edna-use-inheritance t)
  (org-edna-mode 1)
  )
