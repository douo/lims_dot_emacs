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
(defvar my/tui-kkp-enable-timer nil
  "Pending timer used to enable KKP after TTY startup settles.")

(defun my/tui-kkp-active-p (&optional terminal)
  "Return non-nil when KKP is already active in TERMINAL."
  (and (boundp 'kkp--active-terminal-list)
       (memq (or terminal (frame-terminal (selected-frame))) kkp--active-terminal-list)))

(defun my/tui-enable-kkp-now ()
  "Enable kitty keyboard protocol once the selected TTY is ready."
  (setq my/tui-kkp-enable-timer nil)
  (when (and (not noninteractive)
             (not (display-graphic-p))
             (fboundp 'kkp-enable-in-terminal))
    (let ((terminal (frame-terminal (selected-frame))))
      (when (and (terminal-live-p terminal)
                 (not (my/tui-kkp-active-p terminal)))
        ;; If an earlier startup attempt left this guard set, clear it before
        ;; the single delayed enable.  Avoid repeated retries: leftover query
        ;; replies can be inserted into *scratch* as visible "0u6c" text.
        (set-terminal-parameter terminal 'kkp--setup-started nil)
        (global-kkp-mode 1)
        (unless (or (my/tui-kkp-active-p terminal)
                    (terminal-parameter terminal 'kkp--setup-started))
          (kkp-enable-in-terminal terminal))))))

(defun my/tui-schedule-kkp-enable ()
  "Schedule one delayed KKP enable attempt for the current TTY."
  (when (and (not noninteractive)
             (not (display-graphic-p)))
    (when (timerp my/tui-kkp-enable-timer)
      (cancel-timer my/tui-kkp-enable-timer))
    (setq my/tui-kkp-enable-timer
          (run-at-time 0.7 nil #'my/tui-enable-kkp-now))))

(use-package kkp
  :straight t
  :demand t
  :hook ((emacs-startup . my/tui-schedule-kkp-enable)
         (window-setup . my/tui-schedule-kkp-enable))
  :config
  ;; (setq kkp-alt-modifier 'alt) ;; use this if you want to map the Alt keyboard modifier to Alt in Emacs (and not to Meta)
  (my/tui-schedule-kkp-enable)
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
