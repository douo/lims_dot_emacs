;;; init-image-dired.el --- Remote image-dired via SSHFS -*- lexical-binding: t -*-
;;; Commentary:
;;
;; 让内置 `image-dired' 支持预览 TRAMP 远程目录下的图片。
;;
;; 优化方案背景：
;; 1. 问题：默认情况下，内置的 `image-dired' 不支持远程 TRAMP 目录的图片预览
;;    （它会在本地调用 `convert' 处理 TRAMP 路径，导致命令因找不到路径而直接
;;    失败）。在早期的定制尝试中，将其改为远程执行 `convert' 或同步拷贝原图至
;;    本地，又会因为并发的 SSH 隧道请求导致 TRAMP 链接死锁（Emacs 100% CPU
;;    卡死），或者因为同步复制在主线程引起界面严重卡顿。
;; 2. 方案：自动拦截远程 TRAMP 路径，静默在后台通过 SSHFS 将远程目录挂载为本地
;;    目录。这样 Emacs 对其进行纯本地操作，完美规避了 TRAMP 并发死锁与网络传输
;;    带来的卡顿，大大提升了预览生成速度。
;;
;; 跨平台：挂载/卸载的实现因系统而异，全部收敛在本文件的平台辅助函数里。
;;   - macOS：挂载点位于 /Volumes，使用 macFUSE `fskit' 后端，卸载用
;;     `diskutil unmount force'（能卸下僵死挂载并自动删除目录）。
;;   - Linux：挂载点位于 $XDG_RUNTIME_DIR（回退 /tmp），sshfs 不会自动创建
;;     挂载点目录需手动 `mkdir'，卸载用 `fusermount3'/`fusermount -u'，并手动
;;     删除残留空目录。
;;   - 其它系统：不挂载，回退到 `image-dired' 原始行为。
;;
;;; Code:

(require 'tramp)
(require 'cl-lib)

;; image-dired 内置变量，声明以消除本文件被提前 require 时的编译告警。
(defvar image-dired-thumbnail-buffer)

(defgroup my-image-dired-sshfs nil
  "Auto-mount remote TRAMP directories via SSHFS for `image-dired'."
  :group 'image-dired)

(defvar my-image-dired-sshfs-platform
  (cond ((eq system-type 'darwin) 'macos)
        ((eq system-type 'gnu/linux) 'linux)
        (t 'unsupported))
  "当前系统对应的 SSHFS 实现类型：`macos'、`linux' 或 `unsupported'。")

(defvar-local my-image-dired-sshfs-mount nil
  "Buffer-local variable storing the SSHFS mount point for this image-dired buffer.")

(defvar my-image-dired-sshfs--active-mounts nil
  "全局活动挂载点列表。
`kill-buffer-hook' 在退出 Emacs 时不会触发，单靠它无法保证挂载被清理。
这里集中记录所有已挂载的挂载点，供退出时（`kill-emacs-hook'）兜底统一卸载。")


;;; 平台抽象层 ----------------------------------------------------------------

(defun my-image-dired-sshfs--mount-base ()
  "返回本平台用于存放挂载点的基目录，并确保其存在。
macOS 使用 /Volumes（系统已存在）；Linux 使用 $XDG_RUNTIME_DIR/image-dired
（回退 /tmp/image-dired），首次使用时创建。"
  (pcase my-image-dired-sshfs-platform
    ('macos "/Volumes")
    ('linux
     (let ((base (expand-file-name
                  "image-dired"
                  (or (getenv "XDG_RUNTIME_DIR") "/tmp"))))
       (make-directory base t)
       base))
    (_ (error "Unsupported platform for SSHFS image-dired"))))

(defun my-image-dired-sshfs--mount-args (volname)
  "返回挂载 VOLNAME 时附加的 sshfs `-o' 参数列表。
公共参数保证断线重连与僵死探测；macOS 额外使用 fskit 后端与 volname。"
  (let ((common (list "-o" "reconnect"
                      "-o" "ServerAliveInterval=15"
                      "-o" "ServerAliveCountMax=3")))
    (pcase my-image-dired-sshfs-platform
      ('macos
       (append (list "-o" "backend=fskit"
                     "-o" (concat "volname=" volname))
               common))
      ;; Linux 的 sshfs 不认 fskit/volname，保持最小参数即可。
      ('linux common)
      (_ common))))

(defun my-image-dired-sshfs--prepare-mount-point (mount-point)
  "在挂载前为 MOUNT-POINT 做平台相关准备。
Linux 下 sshfs 不会自动创建挂载点目录，需要先建好；macOS 下 fskit 会自行
创建，无需处理。"
  (when (eq my-image-dired-sshfs-platform 'linux)
    (make-directory mount-point t)))

(defun my-image-dired-sshfs--mounted-p (mount-point)
  "判断 MOUNT-POINT 当前是否为一个活动挂载。
不能用 `file-directory-p'：僵死挂载（SSH 断连）下它会因 EPERM 返回 nil，
导致误判为未挂载、进而在同一点叠加挂载。这里直接查 `mount' 表，可靠得多。"
  (with-temp-buffer
    (when (zerop (ignore-errors (call-process "mount" nil t nil)))
      (goto-char (point-min))
      ;; mount 行形如 \"... on /Volumes/xxx (...)\"，按挂载点精确匹配。
      (re-search-forward
       (concat " on " (regexp-quote mount-point) " ") nil t))))

(defun my-image-dired-sshfs--kill-procs (mount-point)
  "Kill any sshfs processes still bound to MOUNT-POINT.
卸载后对应的 sshfs 进程往往不会退出（卡在内核 I/O 中，SIGTERM 也无效），
会变成连着远程的孤儿进程。这里按挂载点精确匹配并 SIGKILL 清理。两平台通用。"
  (when (executable-find "pkill")
    (call-process "pkill" nil nil nil "-9" "-f"
                  (concat "sshfs.*" (regexp-quote mount-point)))))

(defun my-image-dired-sshfs--unmount-command (mount-point)
  "返回卸载 MOUNT-POINT 所用的命令列表 (PROGRAM ARGS...)。
macOS 用 `diskutil unmount force'（卸僵死挂载并自动删目录）；Linux 优先
`fusermount3 -u'，回退 `fusermount -u'。"
  (pcase my-image-dired-sshfs-platform
    ('macos (list "diskutil" "unmount" "force" mount-point))
    ('linux
     (let ((fusermount (or (executable-find "fusermount3")
                           (executable-find "fusermount"))))
       (unless fusermount
         (error "Neither fusermount3 nor fusermount found"))
       (list fusermount "-u" mount-point)))
    (_ (error "Unsupported platform for SSHFS image-dired"))))


;;; 卸载 ----------------------------------------------------------------------

(defun my-image-dired-sshfs--remove-dir (mount-point)
  "删除卸载后遗留的空挂载点目录。
平台差异很关键：
- macOS：挂载点位于 root 拥有的 /Volumes 下，普通用户没有写权限，无法
  `delete-directory'；正常情况下由 `diskutil unmount' 在卸载时连目录一并
  移除，无需也无法在此手动删。故跳过。
- Linux：挂载基目录由本模块自己创建、归当前用户所有，卸载后需手动删空目录。
`delete-directory' 只能删空目录，万一挂载仍在会被拒绝，不会误删远程数据。"
  (when (and (eq my-image-dired-sshfs-platform 'linux)
             (file-directory-p mount-point))
    (ignore-errors (delete-directory mount-point))))

(defun my-image-dired-sshfs--post-unmount (mount-point)
  "卸载 MOUNT-POINT 后的善后：清理孤儿进程、删除遗留空目录、移出活动列表。"
  (my-image-dired-sshfs--kill-procs mount-point)
  (my-image-dired-sshfs--remove-dir mount-point)
  (setq my-image-dired-sshfs--active-mounts
        (delete mount-point my-image-dired-sshfs--active-mounts)))

(defun my-image-dired-sshfs--unmount-sync (mount-point)
  "同步卸载 MOUNT-POINT，阻塞直到完成。用于退出 Emacs / 清理遗留挂载。
异步卸载在 `kill-emacs-hook' 中不可靠（进程会随 Emacs 退出被杀），故走同步。"
  (ignore-errors
    (apply #'call-process
           (car (my-image-dired-sshfs--unmount-command mount-point))
           nil nil nil
           (cdr (my-image-dired-sshfs--unmount-command mount-point))))
  (my-image-dired-sshfs--post-unmount mount-point))

(defun my-image-dired-sshfs-unmount ()
  "Unmount the SSHFS volume associated with the current buffer.
卸载命令因平台而异（见 `my-image-dired-sshfs--unmount-command'）。卸载完成
后清理可能残留的 sshfs 孤儿进程，并删除遗留的空挂载点目录（macOS 的 diskutil
通常已自动删除，这里兜底），同时把它移出全局活动挂载列表。"
  (interactive)
  (when (and (boundp 'my-image-dired-sshfs-mount)
             my-image-dired-sshfs-mount)
    (let* ((mount-point my-image-dired-sshfs-mount)
           (command (my-image-dired-sshfs--unmount-command mount-point)))
      (message "Unmounting SSHFS mount: %s" mount-point)
      (setq my-image-dired-sshfs-mount nil)
      (let ((proc (apply #'start-process "sshfs-unmount" nil command)))
        (set-process-sentinel
         proc
         (lambda (process _event)
           (when (eq (process-status process) 'exit)
             (my-image-dired-sshfs--post-unmount mount-point)
             (if (zerop (process-exit-status process))
                 (message "SSHFS unmounted: %s" mount-point)
               (message "SSHFS unmount failed for %s (exit %d)"
                        mount-point
                        (process-exit-status process))))))))))

(defun my-image-dired-sshfs-unmount-all ()
  "同步卸载所有活动挂载。注册到 `kill-emacs-hook' 作为退出兜底。
`kill-buffer-hook' 在退出 Emacs 时不触发，没有这一步，未手动关闭缩略图
buffer 就退出 Emacs 会把挂载永久遗留在挂载基目录下。"
  (dolist (mount-point (copy-sequence my-image-dired-sshfs--active-mounts))
    (my-image-dired-sshfs--unmount-sync mount-point)))

(defun my-image-dired-sshfs--list-stale-mounts ()
  "返回挂载基目录下所有 image-dired-* 挂载点的绝对路径列表。
直接解析 `mount' 表，能抓到 `file-directory-p' 因 EPERM 看不见的僵死挂载。"
  (let* ((base (ignore-errors (my-image-dired-sshfs--mount-base)))
         (prefix (and base (expand-file-name "image-dired-" base)))
         (result nil))
    (when prefix
      (with-temp-buffer
        (when (zerop (ignore-errors (call-process "mount" nil t nil)))
          (goto-char (point-min))
          ;; mount 每行形如 \"<dev> on <mount-point> (<opts>)\"。直接全缓冲搜索
          ;; \" on <prefix>...\"，避免按行切分对超长 macfuse 选项行的脆弱性。
          (let ((pat (concat " on \\(" (regexp-quote prefix) "[^(]*?\\) (")))
            (while (re-search-forward pat nil t)
              ;; 去掉挂载点与 \" (\" 之间可能的尾随空格。
              (push (string-trim-right (match-string 1)) result))))))
    (nreverse result)))

(defun my-image-dired-sshfs-cleanup-stale ()
  "卸载挂载基目录下遗留的 image-dired 挂载（含已僵死的）。
Emacs 异常退出 / 崩溃，或在僵死挂载上叠加挂载时都会残留；启动时调用以
避免逐渐堆积，也可随时手动 \\[my-image-dired-sshfs-cleanup-stale] 清理。"
  (interactive)
  (when (memq my-image-dired-sshfs-platform '(macos linux))
    (let ((mounts (my-image-dired-sshfs--list-stale-mounts)))
      (dolist (mount-point mounts)
        (my-image-dired-sshfs--unmount-sync mount-point))
      (when (called-interactively-p 'interactive)
        (message "Cleaned up %d stale SSHFS mount(s)" (length mounts))))))


;;; 挂载与 Advice -------------------------------------------------------------

(defun my-image-dired-thumbnail-quit ()
  "Kill the current image-dired thumbnail buffer and its window."
  (interactive)
  (quit-window t))

(defun my-image-dired-sshfs-wrapper (orig-fun dirname &rest args)
  "Around advice for `image-dired-show-all-from-dir'.
Auto-mount remote TRAMP DIRNAME via SSHFS before handing a local path to
ORIG-FUN; ARGS are forwarded unchanged."
  (if (and (stringp dirname)
           (tramp-tramp-file-p dirname)
           (not (eq my-image-dired-sshfs-platform 'unsupported)))
      (let* ((parsed (tramp-dissect-file-name dirname))
             (host (tramp-file-name-host parsed))
             (user (tramp-file-name-user parsed))
             (localname (tramp-file-name-localname parsed))
             (remote-path (if (string-prefix-p "~/" localname)
                              (substring localname 2)
                            localname))
             (sshfs-target (if user
                               (concat user "@" host ":" remote-path)
                             (concat host ":" remote-path)))
             (volname (concat "image-dired-" host "-" (substring (md5 dirname) 0 8)))
             (mount-point (expand-file-name
                           volname (my-image-dired-sshfs--mount-base))))

        ;; Ensure any existing mount in the target buffer is cleaned up first
        (let ((existing-buf (get-buffer image-dired-thumbnail-buffer)))
          (when existing-buf
            (with-current-buffer existing-buf
              (when (and (boundp 'my-image-dired-sshfs-mount)
                         my-image-dired-sshfs-mount
                         (not (string= my-image-dired-sshfs-mount mount-point)))
                (my-image-dired-sshfs-unmount)))))

        ;; 若该挂载点已存在挂载（可能是上次遗留、且已僵死），先彻底卸掉再重挂，
        ;; 否则会在僵死挂载上叠加一层，导致卸载后目录仍残留。
        (when (my-image-dired-sshfs--mounted-p mount-point)
          (my-image-dired-sshfs--unmount-sync mount-point))

        ;; Mount the directory if not already mounted
        (unless (my-image-dired-sshfs--mounted-p mount-point)
          (message "Mounting remote path %s via SSHFS to %s..." sshfs-target mount-point)
          (my-image-dired-sshfs--prepare-mount-point mount-point)
          (apply #'start-process "sshfs-mount" nil "sshfs"
                 sshfs-target mount-point
                 (my-image-dired-sshfs--mount-args volname))
          ;; 等待至多 10 秒，直到挂载真正出现在 mount 表里。
          ;; 两个坑：
          ;; 1. 不能用 `file-directory-p' 判断成功——挂载点目录可能在挂载完成
          ;;    前就已存在（Linux 预创建；macOS 失败也会留下空目录），会让等待
          ;;    立即误判成功，把空目录当作挂载结果。故改查 `mount' 表。
          ;; 2. 不能依赖 sshfs 进程是否存活——挂载成功后 sshfs 会 daemon 化、
          ;;    前台进程随即退出，用进程状态判断会过早判失败。故只轮询 mount 表。
          (let ((counter 0))
            (while (and (not (my-image-dired-sshfs--mounted-p mount-point))
                        (< counter 100))
              (sleep-for 0.1)
              (setq counter (1+ counter))))
          (unless (my-image-dired-sshfs--mounted-p mount-point)
            ;; 挂载失败：清理可能残留的孤儿进程与空目录，再报错。
            (my-image-dired-sshfs--kill-procs mount-point)
            (my-image-dired-sshfs--remove-dir mount-point)
            (error "Failed to mount %s via SSHFS: mount did not appear in mount table"
                   sshfs-target)))

        ;; Call original function with the local mount point
        (apply orig-fun (cons mount-point args))

        ;; 记入全局活动列表，供退出 Emacs 时（`kill-emacs-hook'）兜底卸载。
        (cl-pushnew mount-point my-image-dired-sshfs--active-mounts :test #'string=)

        ;; Hook the unmount process to the thumbnail buffer
        (let ((thumb-buf (get-buffer image-dired-thumbnail-buffer)))
          (when thumb-buf
            (with-current-buffer thumb-buf
              (setq-local my-image-dired-sshfs-mount mount-point)
              (add-hook 'kill-buffer-hook #'my-image-dired-sshfs-unmount nil t)))))

    ;; If not a TRAMP path (or platform unsupported), run the original function normally
    (apply orig-fun (cons dirname args))))


;;; image-dired 配置 ----------------------------------------------------------

(use-package image-dired
  ;; image-dired 是 Emacs 内置的
  :after dired
  :bind
  (:map image-dired-thumbnail-mode-map
        ("n" . image-dired-display-next)
        ("p" . image-dired-display-previous)
        ("q" . my-image-dired-thumbnail-quit))
  :config
  (advice-add 'image-dired-show-all-from-dir :around #'my-image-dired-sshfs-wrapper))

;; 退出 Emacs 时兜底卸载所有活动挂载（`kill-buffer-hook' 在退出时不触发）。
(add-hook 'kill-emacs-hook #'my-image-dired-sshfs-unmount-all)

;; 启动时清理上次会话遗留（异常退出 / 崩溃）的挂载，避免逐渐堆积。
(my-image-dired-sshfs-cleanup-stale)

(provide 'init-image-dired)

;;; init-image-dired.el ends here
