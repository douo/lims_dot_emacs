;;; sis-prefix-override-test.el --- Tests for global SIS prefix handling -*- lexical-binding: t; -*-

(require 'ert)
(require 'calendar)
(require 'org-agenda)
(require 'sis)

(ert-deftest douo/sis-prefix-override-clears-existing-buffer-disable ()
  (let ((buffers (mapcar #'generate-new-buffer
                         '(" *sis-test-ordinary*" "*sis-test-special*"))))
    (unwind-protect
        (progn
          (dolist (buffer buffers)
            (with-current-buffer buffer
              (setq-local sis--prefix-override-map-enable nil)))
          (douo/sis-enable-prefix-override-everywhere)
          (should (null sis-prefix-override-buffer-disable-predicates))
          (should (default-value 'sis--prefix-override-map-enable))
          (dolist (buffer buffers)
            (with-current-buffer buffer
              (should-not
               (local-variable-p 'sis--prefix-override-map-enable)))))
      (mapc #'kill-buffer buffers))))

(ert-deftest douo/sis-prefix-override-stays-enabled-in-special-modes ()
  (dolist (spec '(("ordinary" fundamental-mode)
                  ("*Calendar SIS test*" calendar-mode)
                  ("*Org Agenda SIS test*" org-agenda-mode)))
    (pcase-let ((`(,name ,mode) spec))
      (let ((buffer (generate-new-buffer name)))
        (unwind-protect
            (with-current-buffer buffer
              (funcall mode)
              (sis--respect-post-cmd-timer-fn)
              (should-not
               (local-variable-p 'sis--prefix-override-map-enable))
              (should sis--prefix-override-map-enable)
              (dolist (prefix sis-prefix-override-keys)
                (should (eq (key-binding (kbd prefix))
                            #'sis--prefix-override-handler))))
          (kill-buffer buffer))))))

;;; sis-prefix-override-test.el ends here
