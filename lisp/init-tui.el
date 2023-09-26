;;; init-tui.el --- TUI configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package corfu-terminal
  :straight '(corfu-terminal
              :type git
              :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
  :after corfu
  :config
  (corfu-terminal-mode +1))

(load-theme 'modus-vivendi)

(provide 'init-tui)

;;; init-tui.el ends here
