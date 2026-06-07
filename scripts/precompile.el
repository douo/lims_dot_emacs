;;; precompile.el --- 预编译 Emacs 包和本地配置 -*- lexical-binding: t; -*-

;; 用法：
;;   emacs -Q --batch -l scripts/precompile.el
;;   emacs -Q --batch -l scripts/precompile.el -- --help
;;   emacs -Q --batch -l scripts/precompile.el -- --dry-run
;;   emacs -Q --batch -l scripts/precompile.el -- --dry-run --list
;;   emacs -Q --batch -l scripts/precompile.el -- --force
;;
;; 注意：这个脚本只在独立的批处理 Emacs 进程中运行，不会连接或重载正在使用的 Emacs。
;; 预编译产物是 native-comp 的 .eln 文件，默认写入 `eln-cache'，不生成本地配置的 .elc。
;; 更新包、切换 Emacs 版本、或者大量修改配置后，可以手动运行一次。

(setq user-emacs-directory
      (file-name-as-directory
       (expand-file-name
        (or (getenv "EMACS_USER_DIRECTORY")
            (file-name-directory
             (directory-file-name
              (file-name-directory (or load-file-name buffer-file-name))))))))

(setq native-comp-async-report-warnings-errors 'silent
      native-comp-jit-compilation nil
      native-comp-deferred-compilation nil)

(require 'cl-lib)
(require 'subr-x)
(require 'comp)

(defvar douo/precompile-dry-run nil)
(defvar douo/precompile-force nil)
(defvar douo/precompile-list nil)
(defvar douo/precompile-help nil)

(defun douo/precompile-print-help ()
  "输出预编译脚本帮助。"
  (princ
   (concat
    "预编译 Emacs 包和本地配置\n\n"
    "用法：\n"
    "  emacs -Q --batch -l scripts/precompile.el\n"
    "  emacs -Q --batch -l scripts/precompile.el -- --dry-run\n"
    "  emacs -Q --batch -l scripts/precompile.el -- --dry-run --list\n"
    "  emacs -Q --batch -l scripts/precompile.el -- --force\n\n"
    "参数：\n"
    "  --help, -h   显示这份帮助。\n"
    "  --dry-run    只统计需要预编译的文件，不生成 .eln。\n"
    "  --list       配合 --dry-run 列出计划预编译的文件。\n"
    "  --force      忽略已有 .eln，强制重新预编译。\n\n"
    "说明：\n"
    "  这个脚本只在独立批处理 Emacs 进程中运行，不连接当前 Emacs。\n"
    "  产物写入 native-comp 的 eln-cache，用于减少懒加载功能首次打开时的卡顿。\n"
    "  更新包、切换 Emacs 版本、或者大量修改配置后，建议手动运行一次。\n")))

(defun douo/precompile-parse-args ()
  "解析预编译脚本参数。"
  (dolist (arg command-line-args-left)
    (pcase arg
      ("--" nil)
      ((or "--help" "-h") (setq douo/precompile-help t))
      ("--dry-run" (setq douo/precompile-dry-run t))
      ("--force" (setq douo/precompile-force t))
      ("--list" (setq douo/precompile-list t))
      (_ (message "忽略未知参数：%s" arg)))))

(defun douo/precompile-directory-files (directory)
  "递归收集 DIRECTORY 下的 Emacs Lisp 文件。"
  (when (file-directory-p directory)
    (directory-files-recursively directory "\\.el\\'")))

(defun douo/precompile-build-files ()
  "收集 straight 构建目录里的包文件。"
  (let ((build-dir (expand-file-name "straight/build" user-emacs-directory)))
    (douo/precompile-directory-files build-dir)))

(defun douo/precompile-local-files ()
  "收集本地配置文件。"
  (let ((lisp-dir (expand-file-name "lisp" user-emacs-directory)))
    (append
     (douo/precompile-directory-files lisp-dir)
     ;; 注意：默认不编译 init.el 和 early-init.el。
     ;; 这两个文件面向启动过程，直接加载比提前编译更容易保持行为可预期。
     nil)))

(defun douo/precompile-readable-path (file)
  "返回 FILE 相对 `user-emacs-directory' 的可读路径。"
  (file-relative-name file user-emacs-directory))

(defun douo/precompile-setup-load-path ()
  "设置预编译时需要的加载路径。"
  (let ((build-dir (expand-file-name "straight/build" user-emacs-directory))
        (lisp-dir (expand-file-name "lisp" user-emacs-directory)))
    (when (file-directory-p build-dir)
      (dolist (dir (directory-files build-dir t directory-files-no-dot-files-regexp))
        (when (file-directory-p dir)
          (add-to-list 'load-path dir))))
    (when (file-directory-p lisp-dir)
      (add-to-list 'load-path lisp-dir))
    ;; 注意：本地 lisp 配置里有 use-package 声明，预编译前需要先提供宏环境。
    ;; straight 只作为 use-package 的 :straight 关键字支持，不会在这里更新或安装包。
    (require 'straight nil t)
    (require 'use-package nil t)))

(defun douo/precompile-needs-compile-p (file)
  "判断 FILE 是否需要重新生成 .eln。"
  (or douo/precompile-force
      (let ((eln-file (comp-el-to-eln-filename file)))
        (file-newer-than-file-p file eln-file))))

(defun douo/precompile-file (file)
  "同步预编译 FILE，返回结果 plist。"
  (let ((relative (douo/precompile-readable-path file)))
    (cond
     ((not (douo/precompile-needs-compile-p file))
      (list :status 'skipped :file relative))
     (douo/precompile-dry-run
      (when douo/precompile-list
        (message "计划预编译：%s" relative))
      (list :status 'planned :file relative))
     (t
      (message "预编译：%s" relative)
      (condition-case err
          (let ((output (native-compile file)))
            (list :status 'compiled :file relative :output output))
        (error
         (list :status 'failed :file relative :error err)))))))

(defun douo/precompile-print-summary (results elapsed)
  "输出 RESULTS 的汇总信息，ELAPSED 是耗时秒数。"
  (let ((compiled 0)
        (planned 0)
        (skipped 0)
        (failed nil))
    (dolist (result results)
      (pcase (plist-get result :status)
        ('compiled (cl-incf compiled))
        ('planned (cl-incf planned))
        ('skipped (cl-incf skipped))
        ('failed (push result failed))))
    (message "预编译汇总：compiled=%d planned=%d skipped=%d failed=%d elapsed=%.2fs"
             compiled planned skipped (length failed) elapsed)
    (when failed
      (message "失败文件：")
      (dolist (result (nreverse failed))
        (message "  %s: %S"
                 (plist-get result :file)
                 (plist-get result :error)))
      (kill-emacs 1))))

(defun douo/precompile-main ()
  "执行预编译。"
  (douo/precompile-parse-args)
  (when douo/precompile-help
    (douo/precompile-print-help)
    (kill-emacs 0))
  (douo/precompile-setup-load-path)
  (let* ((start-time (current-time))
         (files (delete-dups
                 (mapcar #'file-truename
                         (append (douo/precompile-build-files)
                                 (douo/precompile-local-files)))))
         (results nil))
    (message "预编译目标：files=%d dry-run=%S force=%S"
             (length files) douo/precompile-dry-run douo/precompile-force)
    (dolist (file (sort files #'string<))
      (push (douo/precompile-file file) results))
    (douo/precompile-print-summary
     (nreverse results)
     (float-time (time-subtract (current-time) start-time)))))

(douo/precompile-main)
