;;; init-rvid.el --- Remote video playback actions -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Keep remote video playback independent from rimg and Image-Dired.  The
;; command `rvid-play' accepts file targets produced by Embark, Dired, minibuffer
;; completion, Consult, and other sources without copying TRAMP files.

;;; Code:

(require 'subr-x)

(defvar embark-exporters-alist)
(defvar embark-file-map)
(defvar embark-keymap-alist)
(defvar embark-transformer-alist)
(declare-function embark-export-dired "embark" (files))

(defcustom my-rvid-embark-video-extensions
  '("3g2" "3gp" "avi" "flv" "m2ts" "m4v" "mkv" "mov" "mp4"
    "mpeg" "mpg" "ogv" "webm" "wmv")
  "File-name extensions classified as remote video targets by Embark.
Classification is case-insensitive and performs no remote file access."
  :type '(repeat string)
  :group 'rvid)

(defvar my-rvid--embark-base-file-transformer nil
  "Embark's file transformer wrapped by rvid target classification.")

(defun my-rvid--embark-remote-video-p (file)
  "Return non-nil when FILE names a supported remote video target."
  (and (stringp file)
       (file-remote-p file)
       (when-let* ((extension (file-name-extension file)))
         (member (downcase extension) my-rvid-embark-video-extensions))))

(defun my-rvid--embark-file-transformer (type target)
  "Preserve Embark's TYPE/TARGET transform and classify remote videos."
  (let* ((transformed
          (if my-rvid--embark-base-file-transformer
              (funcall my-rvid--embark-base-file-transformer type target)
            (cons type target)))
         (transformed-target (cdr transformed)))
    (if (my-rvid--embark-remote-video-p transformed-target)
        (cons 'rvid-remote-video transformed-target)
      transformed)))

(use-package rvid
  :load-path "~/playground/rimg/emacs"
  :custom
  (rvid-player-backend 'auto)
  :commands (rvid-play
             rvid-play-in-emacs
             rvid-stop
             rvid-toggle-pause
             rvid-seek-forward
             rvid-seek-backward
             rvid-connect
             rvid-disconnect
             rvid-reconnect))

(with-eval-after-load 'embark
  ;; Preserve whichever file transformer was installed before this module and
  ;; wrap it idempotently, including when this file is evaluated again.
  (unless (eq (alist-get 'file embark-transformer-alist)
              #'my-rvid--embark-file-transformer)
    (setq my-rvid--embark-base-file-transformer
          (alist-get 'file embark-transformer-alist)))
  (setf (alist-get 'file embark-transformer-alist)
        #'my-rvid--embark-file-transformer)

  (defvar-keymap my-rvid-embark-remote-video-map
    :doc "Embark actions for remote video file targets."
    :parent embark-file-map
    "V" #'rvid-play)

  (setf (alist-get 'rvid-remote-video embark-keymap-alist)
        'my-rvid-embark-remote-video-map)
  ;; Retain file-style export behavior for the refined target type.
  (setf (alist-get 'rvid-remote-video embark-exporters-alist)
        #'embark-export-dired))

(provide 'init-rvid)

;;; init-rvid.el ends here
