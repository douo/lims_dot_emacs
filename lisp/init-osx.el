;;; init-osx.el --- macOS specific settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; macOS Fix
;; FIXME to_be_deleted
;; 交给 wm(yabai) 处理
;; (with-system darwin
;;   ;; 规避 macOS child-frame 全屏黑屏
;;   ;; https://emacs-china.org/t/mac/11848/8
;;   (if (featurep 'cocoa)
;;       (progn
;;         (setq ns-use-native-fullscreen nil)
;;         (setq ns-use-fullscreen-animation nil)

;;         (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
;;         (menu-bar-mode t)
;;         (run-at-time "2sec" nil
;;                      (lambda ()
;;                        (toggle-frame-fullscreen)
;;                        )))
;;     (require 'fullscreen)
;;     (fullscreen))
;;   )

;; 设置 emacsformacosx option 作为 meta 键
(setq mac-option-modifier 'meta)

(provide 'init-osx)

;;; init-osx.el ends here
