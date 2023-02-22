;; (add-to-list 'load-path (concat user-emacs-directory "lisp/lsp-bridge"))
(use-package lsp-bridge
  :ensure nil
  :load-path  "lisp/lsp-bridge"
  :hook
  (prog-mode . lsp-bridge-mode)
  :bind (:map lsp-bridge-mode-map
              ("M-." . lsp-bridge-find-def)
              ("M-," . lsp-bridge-return-from-def)
              ("M-?" . lsp-bridge-find-references)
              ("C-c h" . lsp-bridge-lookup-documentation)
              ("<up>" . lsp-bridge-popup-documentation-scroll-up)
              ("<down>" . lsp-bridge-popup-documentation-scroll-down)
              ;; ("C-c M-f" . lsp-bridge-code-format)  ;; replace by blacken
              ("C-c M-." . lsp-bridge-diagnostic-jump-next)
              ("C-c M-," . lsp-bridge-diagnostic-jump-prev)
              ("C-c M-?" . lsp-bridge-list-diagnostics)
              )
  :custom
  (acm-candidate-match-function 'orderless-flex)
  (lsp-bridge-python-lsp-server douo/python-lsp-server)
  )

(use-package pyvenv
  :ensure t
  :config
  ;; https://github.com/manateelazycat/lsp-bridge/wiki/Python-virtualenv
  (defun local/lsp-bridge-get-single-lang-server-by-project (project-path filepath)
    (let* (
           (json-object-type 'plist)
           (custom-dir
            (expand-file-name (concat ".cache/lsp-bridge/" douo/python-lsp-server)
                              user-emacs-directory))
           (custom-config
            (expand-file-name (concat douo/python-lsp-server ".json")
                              custom-dir))
           (default-config
            (json-read-file
             (expand-file-name (concat "custom-langserver/" douo/python-lsp-server ".json")
                               user-emacs-directory)))
           (settings (plist-get default-config :settings))
           )
      (plist-put settings :pythonPath (executable-find "python"))
      (make-directory (file-name-directory custom-config) t)
      (message (json-encode default-config))
      (with-temp-file custom-config
        (insert (json-encode default-config)))

      custom-config))

  (add-hook 'python-mode-hook (lambda () (setq-local lsp-bridge-get-single-lang-server-by-project 'local/lsp-bridge-get-single-lang-server-by-project)))

  (add-hook 'pyvenv-post-activate-hooks
            (lambda ()
              (lsp-bridge-restart-process)))
  )
