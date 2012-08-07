;; 来自  http://docs.huihoo.com/homepage/shredderyin/wiki/SimpleConfig.html
;; 括号匹配时显示另外一边的括号，而不是烦人的跳到另一个括号
(show-paren-mode t)
(setq show-paren-style 'parentheses)
;; 在标题栏显示file的名字
(setq frame-title-format "emacs@%f")


;; 内建 ido
(require 'ido)
(require 'php-mode)