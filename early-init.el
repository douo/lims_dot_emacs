(setq package-enable-at-startup nil)

; Emacs GC 简述
;; `gc-cons-threshold' 两次 gc 之间的新增内存 bytes 数 (默认 800KB)
;; `gc-cons-percentage' 新增内存占上次 gc 内存的百分比 (默认 0.1)
;; 当新增内存超过 `gc-cons-threshold' 或者 `gc-cons-percentage' 中较大的一个时会触发 mark-and-sweep gc
;; 两个值只有较大的一个会产生影响，也就是说当内存作用超过 8MB 后，`gc-cons-threshold' 就没有意义了
;; 来自 https://emacsconf.org/2023/talks/gc/ 的建议
;; `gc-cons-threshold' 在初始化期间增加有意义，确实能加快初始化速度
;; `gc-cons-threshold' 超过 80MB 后基本没有意义
(setq gc-cons-threshold (* 80 1024 1024))
;; 调整后初始化后立刻查看 `gc-elapsed' 和 `gcs-done' 的值对比如下：
;; `gc-elapsed' 0.79 ==> 0.15
;; `gcs-done' 68 ==> 10
;; 在初始化完成后恢复默认值
;; 对于大部分人来说无需调整 gc 默认值
;; 除非能明显注意到 gc 导致的卡顿
(defun efs/display-startup-time ()
  (insert (format ";; Emacs loaded in %.2f seconds with %d gc."
                  (float-time
                    (time-subtract after-init-time before-init-time))
           gcs-done)))
(add-hook
 'after-init-hook
 (lambda ()
   (setq gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value)))
   (efs/display-startup-time)
   (makunbound 'my/emacs-start-time)))

;; 检查变量 `server-process' 是否存在可判断是否已经启动了 server
(server-start)
