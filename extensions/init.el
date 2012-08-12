;; 来自  http://docs.huihoo.com/homepage/shredderyin/wiki/SimpleConfig.html
;; 括号匹配时显示另外一边的括号，而不是烦人的跳到另一个括号
(show-paren-mode t)
(setq show-paren-style 'parentheses)
;; 在标题栏显示file的名字
(setq frame-title-format "emacs@%f")
;; 设置默认写入文件的编码为 utf-8-unix
(setq default-buffer-file-coding-system 'utf-8-unix)

;; 内建 ido
(require 'ido)
(require 'php-mode)

;; 加载 markdown 模式
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t) 
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))

;; yaml mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
