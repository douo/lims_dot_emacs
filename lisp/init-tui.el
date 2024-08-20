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

;; tui emacs 支持 kitty keyboard protocol
;; https://sw.kovidgoyal.net/kitty/keyboard-protocol/
;; 可以在终端使用 `C-.' 等 keybinding
;; 还能支持 super 、hyper 等 modifier
(use-package kkp
  :straight t
  :config
  ;; (setq kkp-alt-modifier 'alt) ;; use this if you want to map the Alt keyboard modifier to Alt in Emacs (and not to Meta)
  (global-kkp-mode +1))

(load-theme 'modus-vivendi)


(provide 'init-tui)

;;; init-tui.el ends here
