;;; init-windows.el --- windows specific settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq w32-pass-rwindow-to-system nil)
(setq w32-rwindow-modifier 'super) ; Right Windows key
(w32-register-hot-key [s-])

(provide 'init-windows)

;;; init-windows.el ends here
