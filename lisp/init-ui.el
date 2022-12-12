;; init-ui.el --- Initialize version control system configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2016-2022 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; URL: https://github.com/seagle0128/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; UI.
;;
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; (use-package cnfonts
;;   :ensure t
;;   :config
;;   (cnfonts-enable))

(use-package ef-themes
  :ensure t
  :config
  ;; (load-theme 'ef-duo-light 'no-confirm)
  (ef-themes-select 'ef-duo-light)
  ;; (load-theme 'default 'no-confirm)
  )
(use-package all-the-icons
  :after cnfonts
  :if (display-graphic-p))

(defun icon-displayable-p ()
  "Return non-nil if icons are displayable."
  (and    (or (display-graphic-p) (daemonp))
	  (or (featurep 'all-the-icons)
	      (require 'all-the-icons nil t))))
(use-package doom-themes)
;; Show native line numbers if possible, otherwise use `linum'
(if (fboundp 'display-line-numbers-mode)
    (use-package display-line-numbers
      :ensure nil
      :hook ((prog-mode yaml-mode conf-mode) . display-line-numbers-mode)
      :init (setq display-line-numbers-width-start t))
  (use-package linum-off
    :demand t
    :defines linum-format
    :hook (after-init . global-linum-mode)
    :init (setq linum-format "%4d ")
    :config
    ;; Highlight current line number
    (use-package hlinum
      :defines linum-highlight-in-all-buffersp
      :custom-face (linum-highlight-face ((t (:inherit default :background nil :foreground nil))))
      :hook (global-linum-mode . hlinum-activate)
      :init (setq linum-highlight-in-all-buffersp t))))

;; doom modeline
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-icon t
        doom-modeline-height 1
        doom-modeline-buffer-file-name-style 'auto
        doom-modeline-minor-modes t)
  (unless after-init-time
    (setq-default mode-line-format nil))

  :config
  (setq all-the-icons-scale-factor 1.0)
  (set-face-attribute 'mode-line nil :height 100)
  (set-face-attribute 'mode-line-inactive nil :height 100)
  (doom-modeline-mode)
  )

;; Suppress GUI features
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-scratch-message nil)
(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))
;; Display dividers between windows
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(add-hook 'window-setup-hook #'window-divider-mode)
;; Mouse & Smooth Scroll
;; Scroll one line at a time (less "jumpy" than defaults)
(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . hscroll))
        mouse-wheel-scroll-amount-horizontal 1
        mouse-wheel-progressive-speed nil))
(setq scroll-step 1
      scroll-margin 0
      scroll-conservatively 100000
      auto-window-vscroll nil
      scroll-preserve-screen-position t)

;; Good pixel line scrolling
(if (fboundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode t))
(use-package page-break-lines
  :diminish
  :hook (after-init . global-page-break-lines-mode))
;; Use fixed pitch where it's sensible
(use-package mixed-pitch
  :diminish)
(with-no-warnings
  (when sys/macp
    ;; Render thinner fonts
    (setq ns-use-thin-smoothing t)
    ;; Don't open a file in a new frame
    (setq ns-pop-up-frames nil)))
(use-package hide-mode-line
    :hook (((completion-list-mode
             completion-in-region-mode
             eshell-mode
             shell-mode
             term-mode
             vterm-mode
             pdf-annot-list-mode
             flycheck-error-list-mode) . hide-mode-line-mode)))

;; A minor-mode menu for mode-line
(use-package minions
  :hook (doom-modeline-mode . minions-mode))

;; Font setting
;; (defvar font-list
;;   (cond
;;    ((eq system-type 'darwin)
;;     '(("SF Mono" . 11) ("Monaco" . 11) ("Menlo" . 11)))
;;    ((eq system-type 'windows-nt)
;;     '(("Consolas" . 11) ("Cascadia Mono" . 11) ("Source Code Pro" . 11)))
;;    ;; '(("Source Code Pro" . 11)))
;;    (t
;;     '(("Consolas Ligaturized" . 11) ("Source Code Pro" . 11) ("Source Han Serif CN" . 12) ("DejaVu Sans Mono" . 11))))
;;   "List of fonts and sizes.  The first one available will be used.")

;; ;; Set default font before frame creation to make sure the first frame have the correct size
;; (add-to-list 'default-frame-alist (cons 'font (format "%s-%d" (caar font-list) (cdar font-list))))

;; (defun font-installed-p (font)
;;   "Check if the FONT is available."
;;   (find-font (font-spec :name font)))

;; (defun change-font ()
;;   "Change the font of frame from an available `font-list'."
;;   (interactive)
;;   (let* (available-fonts font-name font-size font-set)
;;     (dolist (font font-list (setq available-fonts (nreverse available-fonts)))
;;       (when (font-installed-p (car font))
;;         (push font available-fonts)))
;;     (if (not available-fonts)
;;         (message "No fonts from the chosen set are available")
;;       (if (called-interactively-p 'interactive)
;;           (let* ((chosen (assoc-string (completing-read "What font to use? " available-fonts nil t)
;;                                        available-fonts)))
;;             (setq font-name (car chosen) font-size (read-number "Font size: " (cdr chosen))))
;;         (setq font-name (caar available-fonts) font-size (cdar available-fonts)))
;;       (setq font-set (format "%s-%d" font-name font-size))
;;       (set-frame-font font-set nil t)
;;       (add-to-list 'default-frame-alist (cons 'font font-set)))))

;; (defun change-unicode-font ()
;;   "Setup the Unicode font."
;;   (when (display-graphic-p)
;;     (cl-loop for font in '("Segoe UI Symbol" "Symbola" "Symbol")
;;              when (font-installed-p font)
;;              return (if (< emacs-major-version 27)
;;                         (set-fontset-font "fontset-default" 'unicode font nil 'prepend)
;;                       (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend)))
;;     (cl-loop for font in '("明兰_Mono" "PingFang SC" "Noto Sans Mono CJK SC")
;;              when (font-installed-p font)
;;              return (dolist (charset '(kana han hangul cjk-misc bopomofo))
;;                       (set-fontset-font t charset font)))
;;     (cl-loop for font in '("Noto Color Emoji" "Apple Color Emoji" "Segoe UI Emoji")
;;              when (font-installed-p font)
;;              return (set-fontset-font t 'unicode font nil 'append))
;;     (dolist (font '("HanaMinA" "HanaMinB"))
;;       (when (font-installed-p font)
;;         (set-fontset-font t 'unicode font nil 'append)))))

;; Run after startup
;; (dolist (fn '(change-font change-unicode-font))
;;   (add-hook 'after-init-hook fn))
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))
(defun centaur-setup-fonts ()
  "Setup fonts."
  (when (display-graphic-p)
    ;; Set default font
    (cl-loop for font in '("Cascadia Code" "Fira Code" "Jetbrains Mono"
                           "SF Mono" "Hack" "Source Code Pro" "Menlo"
                           "Monaco" "DejaVu Sans Mono" "Consolas")
             when (font-installed-p font)
             return (set-face-attribute 'default nil
                                        :family font
                                        :height (cond (sys/macp 120)
                                                      (sys/win32p 100)
                                                      (t 100))))

    ;; Set mode-line font
    ;; (cl-loop for font in '("Menlo" "SF Pro Display" "Helvetica")
    ;;          when (font-installed-p font)
    ;;          return (progn
    ;;                   (set-face-attribute 'mode-line nil :family font :height 120)
    ;;                   (when (facep 'mode-line-active)
    ;;                     (set-face-attribute 'mode-line-active nil :family font :height 120))
    ;;                   (set-face-attribute 'mode-line-inactive nil :family font :height 120)))

    ;; Specify font for all unicode characters
    (cl-loop for font in '("Segoe UI Symbol" "Symbola" "Symbol")
             when (font-installed-p font)
             return (if (< emacs-major-version 27)
                        (set-fontset-font "fontset-default" 'unicode font nil 'prepend)
                      (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend)))

    ;; Emoji
    (cl-loop for font in '("Noto Color Emoji" "Apple Color Emoji" "Segoe UI Emoji")
             when (font-installed-p font)
             return (cond
                     ((< emacs-major-version 27)
                      (set-fontset-font "fontset-default" 'unicode font nil 'prepend))
                     ((< emacs-major-version 28)
                      (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend))
                     (t
                      (set-fontset-font t 'emoji (font-spec :family font) nil 'prepend))))

    ;; Specify font for Chinese characters
    (cl-loop for font in '("明兰_Mono" "WenQuanYi Micro Hei" "PingFang SC" "Microsoft Yahei" "STFangsong")
             when (font-installed-p font)
             return (progn
                      (setq face-font-rescale-alist `((,font . 1.1)))
                      (set-fontset-font t '(#x4e00 . #x9fff) (font-spec :family font))
                      (set-fontset-font t '(#xf0e7 . #x9fff) (font-spec :family font))))))
(centaur-setup-fonts)
(add-hook 'window-setup-hook #'centaur-setup-fonts)
(add-hook 'server-after-make-frame-hook #'centaur-setup-fonts)

(provide 'init-ui)
