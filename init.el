;; -*- lexical-binding: t -*-
;; (setq debug-on-error 1)
(setq warning-minimum-level :error)
(setq native-comp-async-report-warnings-errors nil)
(cond
 ((string-match "-[Mm]icrosoft" operating-system-release)
  (setq own-org-directory "~/org/"))
 ((eq system-type 'darwin)
  (setq own-org-directory "~/org/"))
 ((eq system-type 'windows-nt)
  (setq own-org-directory "~/org/"))
 (t
  (setq own-org-directory "~/org/")))

;; Speed up startup
(setq auto-mode-case-fold nil)
(setq truncate-partial-width-windows nil)
(defconst sys/wsl
  (string-match "-[Mm]icrosoft" operating-system-release))

(defconst sys/win32p
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")
(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst sys/mac-x-p
  (and (display-graphic-p) sys/macp)
  "Are we running under X on a Mac system?")

(defconst sys/linux-x-p
  (and (display-graphic-p) sys/linuxp)
  "Are we running under X on a GNU/Linux system?")

(defconst sys/mac-ns-p
  (eq window-system 'ns)
  "Are we running on a GNUstep or Macintosh Cocoa display?")

(defun childframe-workable-p ()
  "Whether childframe is workable."
  (or (not (or noninteractive
               emacs-basic-display
               (not (display-graphic-p))))
      (daemonp)))

(unless (or (daemonp) noninteractive init-file-debug)
  (let ((old-file-name-handler-alist file-name-handler-alist))
    (setq file-name-handler-alist nil)
    (add-hook 'emacs-startup-hook
              (lambda ()
                "Recover file name handlers."
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist
                                           old-file-name-handler-alist)))))))

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)
(add-hook 'emacs-startup-hook
          (lambda ()
            "Recover GC values after startup."
            (setq gc-cons-threshold 800000
                  gc-cons-percentage 0.1)))

;; Suppress flashing at startup
(setq-default inhibit-redisplay t
              inhibit-message t)
(add-hook 'window-setup-hook
          (lambda ()
            (setq-default inhibit-redisplay nil
                          inhibit-message nil)
            (redisplay)))

(if (eq system-type 'darwin)
    (setenv "LANG" "en_US.UTF-8")
  )

(setq system-time-locale "C")
(format-time-string "%Y-%m-%d %a")

(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("site-lisp" "lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(update-load-path)

(require 'init-basic)
(require 'init-ui)
(require 'init-edit)
(require 'init-utils)
(require 'init-org)
(require 'init-latex)
(require 'init-vcs)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#fff8f0" :foreground "#222222" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 140 :width normal :foundry "ADBO" :family "Source Code Pro")))))

;; Local Variables:
;; jinx-languages: "en"
;; End:
