(use-package corfu-terminal
  :straight '(corfu-terminal
             :type git
             :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
  :after corfu
  :config
  (corfu-terminal-mode +1))

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

(use-package consult-eglot
  :straight (consult-eglot
             :type git
             :host github
             :repo "mohkale/consult-eglot")
  :after (consult eglot)
  :config
  ;; 在 eglot 模式激活时将 xref-find-apropos 映射到 consult-eglot-symbols
  ;; 默认快捷键 C-M-.
   (define-key eglot-mode-map [remap xref-find-apropos] 'consult-eglot-symbols)
   )


(load-theme 'modus-vivendi)
