;; 在windows下，右 win 用作 super
;; Except Win+G and Win+L
(setq w32-pass-rwindow-to-system nil)
(setq w32-rwindow-modifier 'super) ; Right Windows key
(w32-register-hot-key [s-])
