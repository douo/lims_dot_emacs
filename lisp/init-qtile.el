;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Emacs 与 Qtile 窗口管理器深度集成配置 (守护进程兼容版)
;;;
;;; 作者: Gemini & tiou
;;; 功能:
;;;   1. `C-x 5 o` (`other-frame`): 实现跨 qtile group (工作区) 切换 Emacs Frame。
;;;   2. `C-x 5 b` (`consult-buffer-other-frame`): 自动将新建的 Emacs Frame 移动到空闲的 qtile group。
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; 环境检测函数 (最终修正版)
;;;
(defun my-is-qtile-session-p ()
  "检查当前是否在 Qtile 会话中。
首先检查 XDG_CURRENT_DESKTOP 环境变量，如果失败，则检查是否有
一个包含 'qtile' 字符串的进程正在运行。"
  (or (string-equal (downcase (or (getenv "XDG_CURRENT_DESKTOP") "")) "qtile")
      ;; 最终修正: 使用 `pgrep -f` 来搜索整个命令行，
      ;; 因为 qtile 通常由 python 启动，进程名是 python。
      ;; `-f` 参数可以匹配到命令行中的 "qtile" 字符串。
      (and (executable-find "pgrep")
           (zerop (call-process "pgrep" nil nil nil "-f" "qtile")))))

;;;
;;; 功能一: 实现跨 Group 的 Frame 切换 (C-x 5 o)
;;;
(defun my-qtile-switch-via-custom-hook (&rest _)
  "通过 qtile 的自定义用户钩子 'switch_to_win' 来切换 Frame。
此函数作为 advice 附加到 `other-frame` 命令之前执行。
它会在执行前先检查当前是否在 Qtile 会话中。"
  ;; 核心修正: 将环境检测移到函数内部，确保每次调用时都重新检查。
  (when (my-is-qtile-session-p)
    (let* ((next-frame (next-frame (selected-frame)))
           (window-id-str (frame-parameter next-frame 'outer-window-id)))
      (when window-id-str
        (message "命令 qtile 切换到 Frame ID: %s" window-id-str)
        (start-process
         "qtile-switch-hook"
         nil
         "qtile" "cmd-obj" "-o" "cmd" "-f" "fire_user_hook"
         "-a" "switch_to_win"
         window-id-str)))))

;;;
;;; 功能二: 自动放置新创建的 Frame
;;;
(defun my-move-new-frame-to-qtile-group (frame)
  "在 Emacs 创建新 Frame 后，调用 qtile 的亲和性函数，将新 Frame 移动到空闲组。
此函数通过 `after-make-frame-functions` 钩子被调用。
它会在执行前先检查当前是否在 Qtile 会话中。"
  ;; 核心修正: 同样将环境检测移到函数内部。
  (when (my-is-qtile-session-p)
    ;; 仅当可见 Frame 总数大于 2 时才执行移动逻辑。
    ;; 这可以避免在只有单个 Frame 的情况下被自动移动(emacs server 启动就有一个 frame ? emacsclient 启动就是第二个)。
    (when (>= (length (visible-frame-list)) 2)
      (when (and (frame-live-p frame)
                 (display-graphic-p frame)
                 (not (eq (frame-parameter frame 'minibuffer) 'only)))
        (let ((window-id-str (frame-parameter frame 'outer-window-id)))
          (when window-id-str
            (message "命令 qtile 移动新 Frame ID: %s 到空闲组" window-id-str)
            (start-process
             "qtile-move-hook"
             nil
             "qtile" "cmd-obj" "-o" "cmd" "-f" "fire_user_hook"
             "-a" "move_to_empty_group_with_affinity"
             window-id-str)))))))

;;;
;;; 注册钩子与 Advice (核心部分)
;;;
;; 终极健壮性修正: 只有当 `qtile` 命令存在时，才注册 advice 和 hook。
;; 这避免了在非 Qtile 系统上加载不必要的配置，而每个函数内部的
;; 实时会话检测，则完美兼容了 Emacs Server/Daemon 模式。
(when (executable-find "qtile")
  (message "检测到 qtile 命令，正在注册深度集成功能...")

  ;; 为 `other-frame` 命令 (C-x 5 o) 添加我们的切换功能。
  (advice-add 'other-frame :before #'my-qtile-switch-via-custom-hook)

  ;; 将我们的新 Frame 放置功能添加到 Emacs 的“创建Frame之后”钩子列表中。
  (add-hook 'after-make-frame-functions #'my-move-new-frame-to-qtile-group)

  (message "Qtile 集成功能加载完毕。"))


(provide 'init-qtile)
