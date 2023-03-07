(use-package pyvenv
  :ensure t
  :config
  )


(use-package flymake
  :ensure t)

(defun douo/update_eglot_pyright_configuraton ()
  (setq eglot-workspace-configuration
        (list (cons ':python (list ':venvPath pyvenv-virtual-env ':pythonPath (executable-find "python")))))
  )

(use-package eglot
  :ensure t
  :config
  (add-to-list
   'eglot-server-programs
   `(python-mode . (lambda(a)
                     `(,(executable-find "python") "-m" "pyright.langserver" "--stdio"))))
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
  :hook
  (python-mode . douo/update_eglot_pyright_configuraton)
  (python-mode . eglot-ensure)
  )
