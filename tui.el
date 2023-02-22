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
   'eglot-server-programs `(python-mode . (lambda(a)
                                            `(,(executable-find "python") "-m" "pyright.langserver" "--stdio")
                                            )))
  (add-hook pyvenv-post-activate-hooks douo/update_eglot_pyright_configuraton)
  :hook
  (python-mode . douo/update_eglot_pyright_configuraton)
  (python-mode . eglot-ensure)
  )
