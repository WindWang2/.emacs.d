;; init-utils.el --- Initialize version control system configurations.	-*- lexical-binding: t -*-

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
;; Utils.
;;

;; Directional window-selection routines
;; Better terminal emulator
;; @see https://github.com/akermu/emacs-libvterm#installation
(when (and module-file-suffix           ; dynamic module
           (executable-find "cmake")
           (executable-find "libtool")
           (executable-find "make"))
  (use-package vterm
    :bind (:map vterm-mode-map
	   ([f9] . (lambda ()
		     (interactive)
		     (and (fboundp 'shell-pop-toggle)
                          (shell-pop-toggle)))))
    :config
    (define-key vterm-mode-map (kbd "C-\\") nil)
    :init (setq vterm-always-compile-module t)))

(use-package winum
  :bind (("C-`" . winum-select-window-by-number)
         ("C-²" . winum-select-window-by-number)
         ("M-0" . winum-select-window-0-or-10)
         ("M-1" . winum-select-window-1)
         ("M-2" . winum-select-window-2)
         ("M-3" . winum-select-window-3)
         ("M-4" . winum-select-window-4)
         ("M-5" . winum-select-window-5)
         ("M-6" . winum-select-window-6)
         ("M-7" . winum-select-window-7)
         ("M-8" . winum-select-window-8)
         ("M-9" . winum-select-window-9))
  :hook (after-init . winum-mode)
  :config
  (setq winum-auto-setup-mode-line t))
(use-package treemacs
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :bind (("C-x t t"   . treemacs)
	 )
  :config
  (setq    treemacs-missing-project-action  'remove
	   treemacs-sorting                 'alphabetic-asc
	   treemacs-follow-after-init       t
	   treemacs-width                   30
	   )
  :config
  (treemacs-follow-mode t)
  )

;; Restore old window configurations
(use-package winner
  :ensure nil
  :commands (winner-undo winner-redo)
  :hook (after-init . winner-mode)
  :init (setq winner-boring-buffers '("*Completions*"
                                      "*Compile-Log*"
                                      "*inferior-lisp*"
                                      "*Fuzzy Completions*"
                                      "*Apropos*"
                                      "*Help*"
                                      "*cvs*"
                                      "*Buffer List*"
                                      "*Ibuffer*"
                                      "*esh command on file*")))

(use-package popper
  :defines popper-echo-dispatch-actions
  :commands popper-group-by-projectile
  :bind (:map popper-mode-map
         ("C-h z" . popper-toggle-latest)
         ("C-<tab>"   . popper-cycle)
         ("C-M-<tab>" . popper-toggle-type))
  :hook (emacs-startup . popper-mode)
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$" "\\*Pp Eval Output\\*$"
          "\\*Compile-Log\\*"
          "\\*Completions\\*"
          "\\*Warnings\\*"
          "\\*Async Shell Command\\*"
          "\\*Apropos\\*"
          "\\*Backtrace\\*"
          "\\*Calendar\\*"
          "\\*Finder\\*"
          "\\*Embark Actions\\*"

          bookmark-bmenu-mode
          comint-mode
          compilation-mode
          help-mode helpful-mode
          tabulated-list-mode
          Buffer-menu-mode

          gnus-article-mode devdocs-mode
          grep-mode occur-mode rg-mode deadgrep-mode ag-mode pt-mode
          ivy-occur-mode ivy-occur-grep-mode
          process-menu-mode list-environment-mode cargo-process-mode
          youdao-dictionary-mode osx-dictionary-mode fanyi-mode

          "^\\*eshell.*\\*.*$" eshell-mode
          "^\\*shell.*\\*.*$"  shell-mode
          "^\\*terminal.*\\*.*$" term-mode
          "^\\*vterm.*\\*.*$"  vterm-mode

          "\\*DAP Templates\\*$" dap-server-log-mode
          "\\*ELP Profiling Restuls\\*" profiler-report-mode
          "\\*Flycheck errors\\*$" " \\*Flycheck checker\\*$"
          "\\*Paradox Report\\*$" "\\*package update results\\*$" "\\*Package-Lint\\*$"
          "\\*[Wo]*Man.*\\*$"
          "\\*ert\\*$" overseer-buffer-mode
          "\\*gud-debug\\*$"
          "\\*lsp-help\\*$" "\\*lsp session\\*$"
          "\\*quickrun\\*$"
          "\\*tldr\\*$"
          "\\*vc-.*\\*$"
          "^\\*elfeed-entry\\*$"
          "^\\*macro expansion\\**"

          "\\*Agenda Commands\\*" "\\*Org Select\\*" "\\*Capture\\*" "^CAPTURE-.*\\.org*"
          "\\*Gofmt Errors\\*$" "\\*Go Test\\*$" godoc-mode
          "\\*docker-containers\\*" "\\*docker-images\\*" "\\*docker-networks\\*" "\\*docker-volumes\\*"
          "\\*prolog\\*" inferior-python-mode inf-ruby-mode swift-repl-mode
          "\\*rustfmt\\*$" rustic-compilation-mode rustic-cargo-clippy-mode
          rustic-cargo-outdated-mode rustic-cargo-test-moed))

  (with-eval-after-load 'projectile
    (setq popper-group-function #'popper-group-by-projectile))

  ;; (when (display-grayscale-p)
  ;;   (setq popper-mode-line
  ;;         '(:eval
  ;;           (format " %s " (all-the-icons-octicon "pin" :height 0.9 :v-adjust 0.0 :face 'mode-line-emphasis)))))

  (setq popper-echo-dispatch-actions t)
  :config
  (popper-echo-mode 1)

  (with-no-warnings
    (defun my-popper-fit-window-height (win)
      "Determine the height of popup window WIN by fitting it to the buffer's content."
      (fit-window-to-buffer
       win
       (floor (frame-height) 3)
       (floor (frame-height) 3)))
    (setq popper-window-height #'my-popper-fit-window-height)

    (defun popper-close-window-hack (&rest _)
      "Close popper window via `C-g'."
      ;; `C-g' can deactivate region
      (when (and (called-interactively-p 'interactive)
                 (not (region-active-p))
                 popper-open-popup-alist)
        (let ((window (caar popper-open-popup-alist)))
          (when (window-live-p window)
            (delete-window window)))))
    (advice-add #'keyboard-quit :before #'popper-close-window-hack)))


(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
         ("C-c C-p" . wdired-change-to-wdired-mode))
  :config
  ;; Always delete and copy recursively
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always)

  (when sys/macp
    ;; Suppress the warning: `ls does not support --dired'.
    (setq dired-use-ls-dired nil)

    (when (executable-find "gls")
      ;; Use GNU ls as `gls' from `coreutils' if available.
      (setq insert-directory-program "gls")))

  (when (or (and sys/macp (executable-find "gls"))
            (and (not sys/macp) (executable-find "ls")))
    ;; Using `insert-directory-program'
    (setq ls-lisp-use-insert-directory-program t)

    ;; Show directory first
    (setq dired-listing-switches "-alh --group-directories-first")

    ;; Quick sort dired buffers via hydra
    (use-package dired-quick-sort
      :bind (:map dired-mode-map
	     ("S" . hydra-dired-quick-sort/body))))

  ;; Show git info in dired
  (use-package dired-git-info
    :bind (:map dired-mode-map
	   (")" . dired-git-info-mode)))

  ;; Allow rsync from dired buffers
  (use-package dired-rsync
    :bind (:map dired-mode-map
	   ("C-c C-r" . dired-rsync)))

  ;; Colourful dired
  (use-package diredfl
    :init (diredfl-global-mode 1))

  ;; Shows icons
  (use-package all-the-icons-dired
    :diminish
    :hook (dired-mode . (lambda ()
                          (when (icon-displayable-p)
                            (all-the-icons-dired-mode))))
    :init (setq all-the-icons-dired-monochrome nil)
    :config
    (with-no-warnings
      (defun my-all-the-icons-dired--refresh ()
        "Display the icons of files in a dired buffer."
        (all-the-icons-dired--remove-all-overlays)
        ;; NOTE: don't display icons it too many items
        (if (<= (count-lines (point-min) (point-max)) 1000)
            (save-excursion
              (goto-char (point-min))
              (while (not (eobp))
                (when (dired-move-to-filename nil)
                  (let ((case-fold-search t))
                    (when-let* ((file (dired-get-filename 'relative 'noerror))
                                (icon (if (file-directory-p file)
                                          (all-the-icons-icon-for-dir
                                           file
                                           :face 'all-the-icons-dired-dir-face
                                           :height 0.9
                                           :v-adjust all-the-icons-dired-v-adjust)
                                        (apply #'all-the-icons-icon-for-file
                                               file
                                               (append
                                                '(:height 0.9)
                                                `(:v-adjust ,all-the-icons-dired-v-adjust)
                                                (when all-the-icons-dired-monochrome
                                                  `(:face ,(face-at-point))))))))
                      (if (member file '("." ".."))
                          (all-the-icons-dired--add-overlay (dired-move-to-filename) "   \t")
                        (all-the-icons-dired--add-overlay (dired-move-to-filename) (concat " " icon "\t"))))))
                (forward-line 1)))
          (message "Not display icons because of too many items.")))
      (advice-add #'all-the-icons-dired--refresh :override #'my-all-the-icons-dired--refresh)))

  ;; Extra Dired functionality
  (use-package dired-aux :ensure nil)
  (use-package dired-x
    :ensure nil
    :demand t
    :config
    (let ((cmd (cond (sys/mac-x-p "open")
                     (sys/linux-x-p "xdg-open")
                     (sys/win32p "start")
                     (t ""))))
      (setq dired-guess-shell-alist-user
            `(("\\.pdf\\'" ,cmd)
              ("\\.docx\\'" ,cmd)
              ("\\.\\(?:djvu\\|eps\\)\\'" ,cmd)
              ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" ,cmd)
              ("\\.\\(?:xcf\\)\\'" ,cmd)
              ("\\.csv\\'" ,cmd)
              ("\\.tex\\'" ,cmd)
              ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
              ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
              ("\\.html?\\'" ,cmd)
              ("\\.md\\'" ,cmd))))

    (setq dired-omit-files
          (concat dired-omit-files
                  "\\|^.DS_Store$\\|^.projectile$\\|^.git*\\|^.svn$\\|^.vscode$\\|\\.js\\.meta$\\|\\.meta$\\|\\.elc$\\|^.emacs.*"))))

;; `find-dired' alternative using `fd'
(when (executable-find "fd")
  (use-package fd-dired))

(use-package fanyi
  :bind (("C-c d f" . fanyi-dwim)
         ("C-c d d" . fanyi-dwim2)
         ("C-c d h" . fanyi-from-history)))
;; Youdao Dictionary
(use-package youdao-dictionary
  :commands youdao-dictionary-play-voice-of-current-word
  :bind (("C-c y" . my-youdao-dictionary-search-at-point)
         ("C-c d Y" . my-youdao-dictionary-search-at-point)
         ("C-c d y" . youdao-dictionary-search)
         :map youdao-dictionary-mode-map
         ("h" . my-youdao-dictionary-help)
         ("?" . my-youdao-dictionary-help))
  :init
  (setq url-automatic-caching t
        youdao-dictionary-use-chinese-word-segmentation t) ; 中文分词

  (defun my-youdao-dictionary-search-at-point ()
    "Search word at point and display result with `posframe', `pos-tip', or buffer."
    (interactive)
    (if (display-graphic-p)
        (if (and (require 'posframe nil t) (posframe-workable-p))
            (youdao-dictionary-search-at-point-posframe)
          (youdao-dictionary-search-at-point-tooltip))
      (youdao-dictionary-search-at-point)))
  :config
  (with-no-warnings
    (defun my-youdao-dictionary--posframe-tip (string)
      "Show STRING using `posframe-show'."
      (unless (and (require 'posframe nil t) (posframe-workable-p))
        (error "Posframe not workable"))

      (if-let ((word (youdao-dictionary--region-or-word)))
          (progn
            (with-current-buffer (get-buffer-create youdao-dictionary-buffer-name)
              (let ((inhibit-read-only t))
                (erase-buffer)
                (youdao-dictionary-mode)
                (insert (propertize "\n" 'face '(:height 0.5)))
                (insert string)
                (insert (propertize "\n" 'face '(:height 0.5)))
                (set (make-local-variable 'youdao-dictionary-current-buffer-word) word)))
            (posframe-show youdao-dictionary-buffer-name
                           :position (point)
                           :left-fringe 16
                           :right-fringe 16
                           :max-width (/ (frame-width) 2)
                           :max-height (/ (frame-height) 2)
                           :background-color (face-background 'tooltip nil t)
                           :internal-border-color (face-background 'posframe-border nil t)
                           :internal-border-width 1)
            (unwind-protect
                (push (read-event) unread-command-events)
              (progn
                (posframe-hide youdao-dictionary-buffer-name)
                (other-frame 0)))
            (message "Nothing to look up"))))
    (advice-add #'youdao-dictionary--posframe-tip :override #'my-youdao-dictionary--posframe-tip)))

;; OSX dictionary
(when sys/macp
  (use-package osx-dictionary
    :bind (("C-c d i" . osx-dictionary-search-input)
           ("C-c d x" . osx-dictionary-search-pointer))))

;; Display available keybindings in popup
(use-package which-key
  :diminish
  :bind ("C-h M-m" . which-key-show-major-mode)
  :hook (after-init . which-key-mode)
  :init (setq which-key-max-description-length 30
              which-key-show-remaining-keys t)
  :config
  (which-key-add-key-based-replacements "C-c !" "flycheck")
  (which-key-add-key-based-replacements "C-c &" "yasnippet")
  (which-key-add-key-based-replacements "C-c c" "counsel")
  (which-key-add-key-based-replacements "C-c n" "org-roam")
  (which-key-add-key-based-replacements "C-c t" "hl-todo")
  (which-key-add-key-based-replacements "C-c v" "ivy-view")
  (which-key-add-key-based-replacements "C-c C-z" "browse")

  (which-key-add-key-based-replacements "C-x RET" "coding-system")
  (which-key-add-key-based-replacements "C-x 8" "unicode")
  (which-key-add-key-based-replacements "C-x @" "modifior")
  (which-key-add-key-based-replacements "C-x X" "edebug")
  (which-key-add-key-based-replacements "C-x a" "abbrev")
  (which-key-add-key-based-replacements "C-x n" "narrow")
  (which-key-add-key-based-replacements "C-x t" "tab")
  (which-key-add-key-based-replacements "C-x C-a" "edebug")


  (which-key-add-major-mode-key-based-replacements 'emacs-lisp-mode
    "C-c ," "overseer")
  (which-key-add-major-mode-key-based-replacements 'python-mode
    "C-c C-t" "python-skeleton"))
;; Search tools
;; Writable `grep' buffer
(use-package wgrep
  :defer t
  :init
  (setq wgrep-auto-save-buffer t
        wgrep-change-readonly-file t))

;; Fast search tool `ripgrep'
(use-package rg
  :defines projectile-command-map
  :hook (after-init . rg-enable-default-bindings)
  :bind (:map rg-global-map
         ("c" . rg-dwim-current-dir)
         ("f" . rg-dwim-current-file)
         ("m" . rg-menu))
  :init (setq rg-group-result t
              rg-show-columns t)
  :config
  (cl-pushnew '("tmpl" . "*.tmpl") rg-custom-type-aliases)

  (with-eval-after-load 'projectile
    (bind-key "s R" #'rg-project projectile-command-map)))

;; Nice writing
(use-package olivetti
  :diminish
  :bind ("<f7>" . olivetti-mode)
  :init (setq olivetti-body-width 0.62))

;; Process
(use-package proced
  :ensure nil
  :init
  (setq-default proced-format 'verbose)
  (setq proced-auto-update-flag t
        proced-auto-update-interval 3))

;; text mode directory tree
;; (use-package ztree
;;   :custom-face
;;   (ztreep-header-face ((t (:inherit diff-header))))
;;   (ztreep-arrow-face ((t (:inherit font-lock-comment-face))))
;;   (ztreep-leaf-face ((t (:inherit diff-index))))
;;   (ztreep-node-face ((t (:inherit font-lock-variable-name-face))))
;;   (ztreep-expand-sign-face ((t (:inherit font-lock-function-name-face))))
;;   (ztreep-diff-header-face ((t (:inherit (diff-header bold)))))
;;   (ztreep-diff-header-small-face ((t (:inherit diff-file-header))))
;;   (ztreep-diff-model-normal-face ((t (:inherit font-lock-doc-face))))
;;   (ztreep-diff-model-ignored-face ((t (:inherit font-lock-doc-face :strike-through t))))
;;   (ztreep-diff-model-diff-face ((t (:inherit diff-removed))))
;;   (ztreep-diff-model-add-face ((t (:inherit diff-nonexistent))))
;;   :bind (:map ztreediff-mode-map
;;          ("C-<f5>" . ztree-hydra/body))
;;   :init (setq ztree-draw-unicode-lines t
;;               ztree-show-number-of-children t))

;; Misc
(use-package copyit)                    ; copy path, url, etc.
(use-package focus)                     ; Focus on the current region
(use-package memory-usage)

(use-package list-environment
  :hook (list-environment-mode . (lambda ()
                                   (setq tabulated-list-format
                                         (vconcat `(("" ,(if (icon-displayable-p) 2 0)))
                                                  tabulated-list-format))
                                   (tabulated-list-init-header)))
  :init
  (with-no-warnings
    (defun my-list-environment-entries ()
      "Generate environment variable entries list for tabulated-list."
      (mapcar (lambda (env)
                (let* ((kv (split-string env "="))
                       (key (car kv))
                       (val (mapconcat #'identity (cdr kv) "=")))
                  (list key (vector
                             (if (icon-displayable-p)
                                 (all-the-icons-octicon "key" :height 0.8 :v-adjust -0.05)
                               "")
                             `(,key face font-lock-keyword-face)
                             `(,val face font-lock-string-face)))))
              process-environment))
    (advice-add #'list-environment-entries :override #'my-list-environment-entries)))

(unless sys/win32p
  (use-package daemons)                 ; system services/daemons
  (use-package tldr))

(use-package evil-nerd-commenter
  :defer t
  :init
  (global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)
  (global-set-key (kbd "C-c l") 'evilnc-quick-comment-or-uncomment-to-the-line)
  (global-set-key (kbd "C-c k") 'evilnc-copy-and-comment-lines)
  (global-set-key (kbd "C-c m") 'evilnc-comment-or-uncomment-paragraphs))

(use-package sis
  ;; :hook
  ;; enable the /follow context/ and /inline region/ mode for specific buffers
  ;; (((text-mode prog-mode) . sis-context-mode)
  ;;  ((text-mode prog-mode) . sis-inline-mode))
  :init
  :bind ("C-\\" . sis-switch)
  :config
  (when sys/win32p
    (sis-ism-lazyman-config nil t 'w32))
  (when sys/macp
    (sis-ism-lazyman-config

     ;; English input source may be: "ABC", "US" or another one.
     ;; "com.apple.keylayout.ABC"
     "com.apple.keylayout.US"

     ;; Other language input source: "rime", "sogou" or another one.
     ;; "im.rime.inputmethod.Squirrel.Rime"
     "com.sogou.inputmethod.sogou.pinyin"))
  ;; enable the /cursor color/ mode
  (sis-global-cursor-color-mode t)
  ;; enable the /respect/ mode
  (sis-global-respect-mode t)
  ;; enable the /context/ mode for all buffers
  (sis-global-context-mode t)
  ;; enable the /inline english/ mode for all buffers
  (sis-global-inline-mode t)
  (setq-default sis-inline-tighten-head-rule 0)
  )

;; Siri shortcusts: for translating
;; -ref: https://emacs-china.org/t/macos/23234, https://emacs-china.org/t/emacs-macos/23268/6
(when sys/macp
  (use-package simple
    :ensure nil
    :bind ("H-t H-t" . my/translate-language-to-zh-or-zh-to-english)
    :config
    ;; Siri Shortcuts: Translate
    (defun my/siri-translate ()
      (interactive)
      (let ((tempfile (make-temp-file "siri-translate-" nil ".txt")))
        (write-region (format "%s" (thing-at-point 'paragraph)) nil tempfile)
        (end-of-paragraph-text) ; jump to end of paragraph
        (shell-command (format "shortcuts run \"Translate File\" -i %s" tempfile)))
      (shell-command "open -b org.gnu.Emacs")
      ;; (shell-command "pbpaste")
      )

    (defun my/siri-translate2english ()
      (interactive)
      (let ((tempfile (make-temp-file "siri-translate-" nil ".txt")))
        (write-region (format "%s" (thing-at-point 'paragraph)) nil tempfile)
        (end-of-paragraph-text) ; jump to end of paragraph
        (shell-command
         (format "shortcuts run \"Translate File 2 English\" -i %s" tempfile)))
      (shell-command "open -b org.gnu.Emacs")
      ;; (shell-command "pbpaste")
      )

    (defun my/translate-language-to-zh-or-zh-to-english ()
      (interactive) ; 测试
      (let ((string (thing-at-point 'paragraph)))
        (if (eq (string-match "\\cC" string) nil)
            (my/siri-translate)
          (my/siri-translate2english)))
      (shell-command "pbpaste")))
  )

(when (not sys/win32p)
  (use-package rime
    :commands (toggle-input-method)
    :defer t
    :custom
    ;; (rime-librime-root "/usr/lib")
    (default-input-method 'rime)
    (rime-user-data-dir "~/.emacs.d/rime")
    (rime-show-candidate 'posframe)
    (rime-show-preedit t)
    (rime-posframe-properties (list :internal-border-width 1))
    (rime-disable-predicates
     '(rime-predicate-prog-in-code-p
       rime-predicate-auto-english-p
       rime-predicate-hydra-p
       rime-predicate-ace-window-p
       rime-predicate-punctuation-after-ascii-p
       rime-predicate-punctuation-line-begin-p
       my/rime-predicate-punctuation-next-char-is-paired-p
       rime-predicate-tex-math-or-command-p
       rime-predicate-org-latex-mode-p
       rime-predicate-current-uppercase-letter-p))
    :init
    ;; 参考 https://github.com/DogLooksGood/emacs-rime/issues/161
    (add-hook 'kill-emacs-hook #'(lambda () (if (fboundp 'rime-lib-finalize)
                                                (rime-lib-finalize))))
    (global-set-key (kbd "C-\\") 'toggle-input-method)
    (defun my/rime-predicate-punctuation-next-char-is-paired-p ()
      (if (not (eq (point) (point-max)))
          (and (rime-predicate-current-input-punctuation-p)
               (not (string-match-p
                     (rx (any "\"\(\[\{"))
                     (buffer-substring (point) (1- (point)))))
               (string-match-p
                (rx (any "\}\]\)\""))
                (buffer-substring (point) (1+ (point)))))
        nil))

    :config
    (define-key rime-mode-map (kbd "M-j") 'rime-force-enable)
    (setq rime-inline-ascii-trigger 'shift-l)
    ;; (define-key rime-active-mode-map (kbd "M-j") 'rime-inline-ascii)
    (if (eq system-type 'darwin)
        (setq rime-librime-root "~/.emacs.d/librime/dist"))
    )
  )

(use-package openwith
  :ensure t
  :init
  :config
  ;; (setq openwith-associations '(("\\.pdf\\'" "open" (file))))
  (when (string-match "-[Mm]icrosoft" operating-system-release)
    ;; WSL: WSL1 has "-Microsoft", WSL2 has "-microsoft-standard"
    (setq openwith-associations '(("\\.pdf\\'" "wslview" (file))))
    )
  (when (eq system-type 'windows-nt)
    (setq openwith-associations
          '(("\\.pdf\\'" "c:/Users/Administrator/AppData/Local/SumatraPDF/SumatraPDF.exe" (file))))
    )
  (when sys/linuxp
    (setq openwith-associations '(("\\.pdf\\'" "evince" (file)))))
  (when sys/macp
    (setq openwith-associations '(("\\.pdf\\'" "open" (file)))))
  :custom
  (openwith-mode t)
  )

(use-package cal-china-x
  :after calendar
  :commands cal-china-x-setup
  :init (cal-china-x-setup)
  :config
  ;; Holidays
  (setq calendar-mark-holidays-flag t
	cal-china-x-important-holidays cal-china-x-chinese-holidays
	cal-china-x-general-holidays '((holiday-lunar 1 15 "元宵节")
                                       (holiday-lunar 7 7 "七夕节")
                                       (holiday-fixed 3 8 "妇女节")
                                       (holiday-fixed 3 12 "植树节")
                                       (holiday-fixed 5 4 "青年节")
                                       (holiday-fixed 6 1 "儿童节")
                                       (holiday-fixed 9 10 "教师节"))
	cal-china-x-birthdays '((holiday-lunar 12 5 "妈妈生日")
				(holiday-lunar 8 25 "爸爸生日")
				(holiday-lunar 9 14 "小常生日")
				(holiday-lunar 1 8 "一一生日"))
	holiday-other-holidays '((holiday-fixed 2 14 "情人节")
				 (holiday-fixed 4 1 "愚人节")
				 (holiday-fixed 12 25 "圣诞节")
				 (holiday-float 5 0 2 "母亲节")
				 (holiday-float 6 0 3 "父亲节")
				 (holiday-float 11 4 4 "感恩节"))
	calendar-holidays (append cal-china-x-important-holidays
                                  cal-china-x-general-holidays
                                  holiday-other-holidays
                                  cal-china-x-birthdays)))
;; Manage and navigate projects
(use-package projectile
  :diminish
  :bind (:map projectile-mode-map
         ("s-t" . projectile-find-file) ; `cmd-t' or `super-t'
         ("C-c p" . projectile-command-map))
  :hook (after-init . projectile-mode)
  :init
  (setq projectile-mode-line-prefix ""
	projectile-sort-order 'recentf
	projectile-use-git-grep t)
  :config
  ;; Use the faster searcher to handle project files: ripgrep `rg'.
  (when (and (not (executable-find "fd"))
             (executable-find "rg"))
    (setq projectile-generic-command
          (let ((rg-cmd ""))
            (dolist (dir projectile-globally-ignored-directories)
              (setq rg-cmd (format "%s --glob '!%s'" rg-cmd dir)))
            (concat "rg -0 --files --color=never --hidden" rg-cmd))))

  ;; Faster searching on Windows
  (when sys/win32p
    (when (or (executable-find "fd") (executable-find "rg"))
      (setq projectile-indexing-method 'alien
            projectile-enable-caching nil))

    ;; HACK: too slow while getting submodule files on Windows
    (setq projectile-git-submodule-command nil))

  ;; Support Perforce project
  (let ((val (or (getenv "P4CONFIG") ".p4config")))
    (add-to-list 'projectile-project-root-files-bottom-up val)))

(use-package flycheck
  :diminish
  :commands flycheck-redefine-standard-error-levels
  :hook (after-init . global-flycheck-mode)
  :init (setq flycheck-global-modes
              '(not text-mode outline-mode fundamental-mode lisp-interaction-mode
                    org-mode diff-mode shell-mode eshell-mode term-mode vterm-mode)
              flycheck-emacs-lisp-load-path 'inherit
              flycheck-indication-mode (if (display-graphic-p)
                                           'right-fringe
					 'right-margin)
              ;; Only check while saving and opening files
              flycheck-check-syntax-automatically '(save mode-enabled))
  :config
  ;; Prettify indication styles
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'flycheck-fringe-bitmap-arrow
      [16 48 112 240 112 48 16] nil nil 'center))
  (flycheck-redefine-standard-error-levels "⏴" 'flycheck-fringe-bitmap-arrow)

  ;;; On Windows, commands run by flycheck may have CRs (\r\n line endings).
;;; Strip them out before parsing.
  (when (eq system-type 'windows-nt)
    (defun flycheck-parse-output (output checker buffer)
      (let ((sanitized-output (replace-regexp-in-string "\r" "" output))
            )
	(funcall (flycheck-checker-get checker 'error-parser) sanitized-output checker buffer)))))



(use-package vertico
  :init
  (vertico-mode))
;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  (setq enable-recursive-minibuffers t))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion))))
  :config
  (use-package pinyinlib)
  (defun orderless-regexp-pinyin (str)
    (setf (car str) (pinyinlib-build-regexp-string (car str)))
    str)
  (advice-add 'orderless-regexp :filter-args #'orderless-regexp-pinyin))

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ("C-s" . consult-line)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key (kbd "M-."))
  (setq consult-narrow-key "<") ;; (kbd "C-+")
  )
(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; dictionary overlay
;; (add-to-list 'load-path "~/github/websocket-bridge/")
;; (require 'websocket-bridge)
;; (add-to-list 'load-path "~/github/dictionary-overlay/")
;; (require 'dictionary-overlay)
(provide 'init-utils)
