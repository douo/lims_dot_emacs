(use-package pyvenv
  :straight t
  :config
  )


(use-package flymake
  :straight t
  :bind (:map flymake-mode-map
              ("C-c M-." . flymake-goto-next-error)
              ("C-c M-," . flymake-goto-prev-error)
              ("C-c M-?" . flymake-show-buffer-diagnostics)
              )

  )

(defun douo/update_eglot_pyright_configuraton ()
  (setq eglot-workspace-configuration
        (list (cons ':python (list ':venvPath pyvenv-virtual-env ':pythonPath (executable-find "python")))))
  )

(use-package eglot
  :straight t
  :config
  (add-to-list
   'eglot-server-programs
   `(python-mode . (lambda(a)
                     `(,(executable-find "pyright-langserver") "--stdio"))))
   (add-to-list
    'eglot-server-programs
    '((c-mode c++-mode)
      . ("clangd"
         "-j=12"
         "--malloc-trim"
         "--background-index"
         "--clang-tidy"
         "--cross-file-rename"
         "--completion-style=detailed"
         "--pch-storage=memory"
         "--function-arg-placeholders"
         "--header-insertion=iwyu")))
   (add-hook 'pyvenv-post-activate-hooks 'douo/update_eglot_pyright_configuraton)
   (diminish 'eldoc-mode "显")
  :hook
  (python-mode . douo/update_eglot_pyright_configuraton)
  (python-mode . eglot-ensure)
  )

(load-theme 'modus-vivendi)
