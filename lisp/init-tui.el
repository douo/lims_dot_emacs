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
;; 可以在终端使用 `C-.' 等 keybinding 还能支持 super 、hyper 等 modifier 接近 gui 的体验
;; "M-[" 无法支持:
;; kkp 是通过发送 escape sequence 来实现的，格式为 `CSI key-code;modifier:3`
;; 其前缀称为 CSI （Control Sequence Introducer）, 也就是 ESC [（转义写法："\x1b\x5b"]
;; "M-[" 刚好是 CSI ，如果绑定这个 kbd 导致 CSI 被消耗，kkp.el 无法识别 escape sequence，导致快捷键无法生效
(use-package kkp
  :straight t
  :config
  ;; (setq kkp-alt-modifier 'alt) ;; use this if you want to map the Alt keyboard modifier to Alt in Emacs (and not to Meta)
  (global-kkp-mode +1)
  ;; alias "M-<backspace>" to "ESC DEL"
  (global-set-key (kbd "M-<backspace>") (lookup-key (current-global-map) (kbd "ESC DEL"))))


(load-theme 'modus-vivendi)


;; 用于在终端中同步剪切板
;; 除一般终端外支持包括 ssh  、tmux 、screen
(use-package clipetty
  :straight t
  :config
  ;; 全局启用
  :hook (after-init . global-clipetty-mode)
  ;; 或者仅对 `M-w' 有效
  ;; :bind ("M-w" . clipetty-kill-ring-save)
  )


(provide 'init-tui)

;;; init-tui.el ends here
