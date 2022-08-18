;; -*- lexical-binding: t -*-
;; (setq debug-on-error 1)
(cond
 ((string-match "-[Mm]icrosoft" operating-system-release)
  (setq own-org-directory "/mnt/c/org/"))
 ((eq system-type 'darwin)
  (setq own-org-directory "~/org/"))
 ((eq system-type 'windows-nt)
  (setq own-org-directory "C:/org/"))
 (t
  (setq own-org-directory "~/org/")))

;; Speed up startup
(setq auto-mode-case-fold nil)
(setq truncate-partial-width-windows nil)
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
;; 

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

(if (eq system-type 'darwin)
    (setenv "LANG" "en_US.UTF-8")
  )

;; 1. Optimize emacs
(global-auto-revert-mode 1)
(setq inhibit-startup-screen t) ;;启动不显示 *GNU Emacs*
(setq auto-save-default nil)
(setq ring-bell-function 'ignore)
(fset 'yes-or-no-p 'y-or-n-p)
(setq make-backup-files nil)
(delete-selection-mode 1)
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-item 10)
;; Precise mode for scroll
(setq pixel-scroll-precision-large-scroll-height 40.0)
(setq pixel-scroll-precision-interpolation-factor 30)
(pixel-scroll-precision-mode t)

;; 2. Use-package
(require 'subr-x)
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-install 'use-package-ensure-system-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)
;; Setup `use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package gnu-elpa-keyring-update)
;; Auto update packages
(use-package auto-package-update
  :init
  (setq auto-package-update-delete-old-versions t
        auto-package-update-hide-results t)
  (defalias 'upgrade-packages #'auto-package-update-now))
(use-package diminish)
(use-package bind-key)
(with-no-warnings
  ;; Key Modifiers
  (cond
   (sys/win32p
    ;; make PC keyboard's Win key or other to type Super or Hyper
    ;; (setq w32-pass-lwindow-to-system nil)
    ;; (setq w32-lwindow-modifier 'super     ; Left Windows key
    ;;       w32-apps-modifier 'hyper)       ; Menu/App key
    (w32-register-hot-key [s-t]))
   (sys/mac-port-p
    ;; Compatible with Emacs Mac port
    (setq mac-option-modifier 'meta
          mac-command-modifier 'super)
    (bind-keys ([(super a)] . mark-whole-buffer)
               ([(super c)] . kill-ring-save)
               ([(super l)] . goto-line)
               ([(super q)] . save-buffers-kill-emacs)
               ([(super s)] . save-buffer)
               ([(super v)] . yank)
               ([(super w)] . delete-frame)
               ([(super z)] . undo))))

  ;; Optimization
  (when sys/win32p
    (setq w32-get-true-file-attributes nil   ; decrease file IO workload
          w32-pipe-read-delay 0              ; faster IPC
          w32-pipe-buffer-size (* 64 1024))) ; read more at a time (was 4K)
  (unless sys/macp
    (setq command-line-ns-option-alist nil))
  (unless sys/linuxp
    (setq command-line-x-option-alist nil))

  ;; Increase how much is read from processes in a single chunk (default is 4kb)
  (setq read-process-output-max #x10000)  ; 64kb

  ;; Don't ping things that look like domain names.
  (setq ffap-machine-p-known 'reject)

  ;; Garbage Collector Magic Hack
  (use-package gcmh
    :diminish
    :init
    (setq gcmh-idle-delay 5
          gcmh-high-cons-threshold #x1000000) ; 16MB
    (gcmh-mode 1)))
;; Encoding
;; UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
;; Explicitly set the prefered coding systems to avoid annoying prompt
;; from emacs (especially on Microsoft Windows)
(prefer-coding-system 'utf-8)

;; 3. UI
(tool-bar-mode -1)
(scroll-bar-mode -1)
(use-package all-the-icons
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

;; Font
;;; Font setting
(defvar font-list
  (cond
   ((eq system-type 'darwin)
    '(("SF Mono" . 13) ("Monaco" . 13) ("Menlo" . 13)))
   ((eq system-type 'windows-nt)
    '(("Cascadia Mono" . 11) ("SF Mono" . 11) ("Source Code Pro" . 12)))
   ;; '(("Source Code Pro" . 11)))
   (t
    '(("SF Mono" . 11) ("Consolas" . 12) ("Cascadia Mono" . 11))))
  "List of fonts and sizes.  The first one available will be used.")

;; Set default font before frame creation to make sure the first frame have the correct size
(add-to-list 'default-frame-alist (cons 'font (format "%s-%d" (caar font-list) (cdar font-list))))

(defun font-installed-p (font)
  "Check if the FONT is available."
  (find-font (font-spec :name font)))

(defun change-font ()
  "Change the font of frame from an available `font-list'."
  (interactive)
  (let* (available-fonts font-name font-size font-set)
    (dolist (font font-list (setq available-fonts (nreverse available-fonts)))
      (when (font-installed-p (car font))
        (push font available-fonts)))
    (if (not available-fonts)
        (message "No fonts from the chosen set are available")
      (if (called-interactively-p 'interactive)
          (let* ((chosen (assoc-string (completing-read "What font to use? " available-fonts nil t)
                                       available-fonts)))
            (setq font-name (car chosen) font-size (read-number "Font size: " (cdr chosen))))
        (setq font-name (caar available-fonts) font-size (cdar available-fonts)))
      (setq font-set (format "%s-%d" font-name font-size))
      (set-frame-font font-set nil t)
      (add-to-list 'default-frame-alist (cons 'font font-set)))))

(defun change-unicode-font ()
  "Setup the Unicode font."
  (when (display-graphic-p)
    (cl-loop for font in '("Microsoft Yahei" "PingFang SC" "Noto Sans Mono CJK SC")
             when (font-installed-p font)
             return (dolist (charset '(kana han hangul cjk-misc bopomofo))
                      (set-fontset-font t charset font)))
    (cl-loop for font in '("Segoe UI Emoji" "Apple Color Emoji" "Noto Color Emoji")
             when (font-installed-p font)
             return (set-fontset-font t 'unicode font nil 'append))
    (dolist (font '("HanaMinA" "HanaMinB"))
      (when (font-installed-p font)
        (set-fontset-font t 'unicode font nil 'append)))))

;; Run after startup
(dolist (fn '(change-font change-unicode-font))
  (add-hook 'after-init-hook fn))

;; 4. edit
;; Automatically reload files was modified by external program
(use-package autorevert
  :ensure nil
  :diminish
  :hook (after-init . global-auto-revert-mode))
;; Minor mode to aggressively keep your code always indented
(use-package aggressive-indent
  :diminish
  :hook ((after-init . global-aggressive-indent-mode)
         ;; WORKAROUND: Disable in big files due to the performance issues
         ;; https://github.com/Malabarba/aggressive-indent-mode/issues/73
         (find-file . (lambda ()
                        (if (> (buffer-size) (* 3000 80))
                            (aggressive-indent-mode -1)))))
  :config
  ;; Disable in some modes
  (dolist (mode '(gitconfig-mode asm-mode web-mode html-mode css-mode go-mode scala-mode prolog-inferior-mode))
    (push mode aggressive-indent-excluded-modes))

  ;; Disable in some commands
  (add-to-list 'aggressive-indent-protected-commands #'delete-trailing-whitespace t)

  ;; Be slightly less aggressive in C/C++/C#/Java/Go/Swift
  (add-to-list 'aggressive-indent-dont-indent-if
               '(and (derived-mode-p 'c-mode 'c++-mode 'csharp-mode
                                     'java-mode 'go-mode 'swift-mode)
                     (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                                         (thing-at-point 'line))))))

;; Show number of matches in mode-line while searching
(use-package anzu
  :diminish
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)
         :map isearch-mode-map
         ([remap isearch-query-replace] . anzu-isearch-query-replace)
         ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :hook (after-init . global-anzu-mode))
;; Redefine M-< and M-> for some modes
(use-package beginend
  :diminish beginend-global-mode
  :hook (after-init . beginend-global-mode)
  :config (mapc (lambda (pair)
                  (diminish (cdr pair)))
                beginend-modes))
;; Automatic parenthesis pairing
(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

;; Edit multiple regions in the same way simultaneously
(use-package iedit
  :defines desktop-minor-mode-table
  :bind (("C-;" . iedit-mode)
         ("C-x r RET" . iedit-rectangle-mode)
         :map isearch-mode-map ("C-;" . iedit-mode-from-isearch)
         :map esc-map ("C-;" . iedit-execute-last-modification)
         :map help-map ("C-;" . iedit-mode-toggle-on-function))
  :config
  ;; Avoid restoring `iedit-mode'
  (with-eval-after-load 'desktop
    (add-to-list 'desktop-minor-mode-table
                 '(iedit-mode nil))))

;; Increase selected region by semantic units
(use-package expand-region
  :bind ("C-=" . er/expand-region))
;; Multiple cursors
(use-package multiple-cursors
  :bind (("C-S-c C-S-c"   . mc/edit-lines)
         ("C->"           . mc/mark-next-like-this)
         ("C-<"           . mc/mark-previous-like-this)
         ("C-c C-<"       . mc/mark-all-like-this)
         ("C-M->"         . mc/skip-to-next-like-this)
         ("C-M-<"         . mc/skip-to-previous-like-this)
         ("s-<mouse-1>"   . mc/add-cursor-on-click)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)
         :map mc/keymap
         ("C-|" . mc/vertical-align-with-space)))

;; Smartly select region, rectangle, multi cursors
(use-package smart-region
  :hook (after-init . smart-region-on))

;; On-the-fly spell checker
(use-package flyspell
  :ensure nil
  :diminish
  :if (executable-find "aspell")
  :hook (((text-mode outline-mode) . flyspell-mode)
         (prog-mode . flyspell-prog-mode)
         (flyspell-mode . (lambda ()
                            (dolist (key '("C-;" "C-," "C-."))
                              (unbind-key key flyspell-mode-map)))))
  :init (setq flyspell-issue-message-flag nil
              ispell-program-name "aspell"
              ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together"))
  :config
  ;; Correcting words with flyspell via Ivy
  ;; (use-package flyspell-correct-ivy
  ;;   :after ivy
  ;;   :bind (:map flyspell-mode-map
  ;;          ([remap flyspell-correct-word-before-point] . flyspell-correct-wrapper))
  ;;   :init (setq flyspell-correct-interface #'flyspell-correct-ivy))
  )

;; Treat undo history as a tree
(use-package undo-tree
  :diminish
  :hook (after-init . global-undo-tree-mode)
  :init
  (setq undo-tree-visualizer-timestamps t
        undo-tree-enable-undo-in-region nil
        undo-tree-auto-save-history nil)

  ;; HACK: keep the diff window
  (with-no-warnings
    (make-variable-buffer-local 'undo-tree-visualizer-diff)
    (setq-default undo-tree-visualizer-diff t)))

;; Open files as another user
(unless sys/win32p
  (use-package sudo-edit))

;; 5. Code
(use-package yasnippet
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode))

(use-package yasnippet-snippets
  :after yasnippet)

;; Prettify Symbols
;; e.g. display “lambda” as “λ”
(use-package prog-mode
  :ensure nil
  :init
  (setq prettify-symbols-unprettify-at-point 'right-edge))

;; Tree-sitter: need dynamic module feature
;; (when (and centaur-tree-sitter
;;            (functionp 'module-load))
;;   (use-package tree-sitter
;;     :ensure tree-sitter-langs
;;     :diminish
;;     :hook ((after-init . global-tree-sitter-mode)
;;            (tree-sitter-after-on . tree-sitter-hl-mode))))

;; Cross-referencing commands
(use-package xref
  :ensure nil
  :init
  (with-no-warnings
    (when (executable-find "rg")
      (setq xref-search-program 'ripgrep))
    (setq xref-show-definitions-function #'xref-show-definitions-completing-read
          xref-show-xrefs-function #'xref-show-definitions-completing-read)
    ;; Select from xref candidates with Ivy
    (use-package ivy-xref
      :after ivy
      :init
      (setq xref-show-definitions-function #'ivy-xref-show-defs)
      (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))))

;; Code styles
(use-package editorconfig
  :diminish
  :hook (after-init . editorconfig-mode))

;; Run commands quickly
(use-package quickrun
  :bind (("C-<f5>" . quickrun)
         ("C-c X" . quickrun)))

;; Emacs lisp mode
(use-package elisp-mode
  :ensure nil
  :defines flycheck-disabled-checkers
  :bind (:map emacs-lisp-mode-map
         ("C-c C-x" . ielm)
         ("C-c C-c" . eval-defun)
         ("C-c C-b" . eval-buffer))
  :hook (emacs-lisp-mode . (lambda ()
                             "Disable the checkdoc checker."
                             (setq-local flycheck-disabled-checkers
                                         '(emacs-lisp-checkdoc))))
  :config
  (when (boundp 'elisp-flymake-byte-compile-load-path)
    (add-to-list 'elisp-flymake-byte-compile-load-path load-path))

  ;; Syntax highlighting of known Elisp symbols
  (use-package highlight-defined
    :hook (emacs-lisp-mode . highlight-defined-mode)
    :init (setq highlight-defined-face-use-itself t))

  (with-no-warnings
    ;; Align indent keywords
    ;; @see https://emacs.stackexchange.com/questions/10230/how-to-indent-keywords-aligned
    (defun my-lisp-indent-function (indent-point state)
      "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.

INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:

* `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);

* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;

* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.

This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
      (let ((normal-indent (current-column))
            (orig-point (point)))
        (goto-char (1+ (elt state 1)))
        (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
        (cond
         ;; car of form doesn't seem to be a symbol, or is a keyword
         ((and (elt state 2)
               (or (not (looking-at "\\sw\\|\\s_"))
                   (looking-at ":")))
          (if (not (> (save-excursion (forward-line 1) (point))
                      calculate-lisp-indent-last-sexp))
              (progn (goto-char calculate-lisp-indent-last-sexp)
                     (beginning-of-line)
                     (parse-partial-sexp (point)
                                         calculate-lisp-indent-last-sexp 0 t)))
          ;; Indent under the list or under the first sexp on the same
          ;; line as calculate-lisp-indent-last-sexp.  Note that first
          ;; thing on that line has to be complete sexp since we are
          ;; inside the innermost containing sexp.
          (backward-prefix-chars)
          (current-column))
         ((and (save-excursion
                 (goto-char indent-point)
                 (skip-syntax-forward " ")
                 (not (looking-at ":")))
               (save-excursion
                 (goto-char orig-point)
                 (looking-at ":")))
          (save-excursion
            (goto-char (+ 2 (elt state 1)))
            (current-column)))
         (t
          (let ((function (buffer-substring (point)
                                            (progn (forward-sexp 1) (point))))
                method)
            (setq method (or (function-get (intern-soft function)
                                           'lisp-indent-function)
                             (get (intern-soft function) 'lisp-indent-hook)))
            (cond ((or (eq method 'defun)
                       (and (null method)
                            (> (length function) 3)
                            (string-match "\\`def" function)))
                   (lisp-indent-defform state indent-point))
                  ((integerp method)
                   (lisp-indent-specform method state
                                         indent-point normal-indent))
                  (method
                   (funcall method indent-point state))))))))
    (add-hook 'emacs-lisp-mode-hook
              (lambda () (setq-local lisp-indent-function #'my-lisp-indent-function)))

    ;; Add remove buttons for advices
    (add-hook 'help-mode-hook 'cursor-sensor-mode)

    (defun function-advices (function)
      "Return FUNCTION's advices."
      (let ((flist (indirect-function function)) advices)
        (while (advice--p flist)
          (setq advices `(,@advices ,(advice--car flist)))
          (setq flist (advice--cdr flist)))
        advices))

    (defun add-remove-advice-button (advice function)
      (when (and advice (symbolp advice))
        (let ((inhibit-read-only t))
          (insert "\t")
          (insert-text-button
           "[Remove]"
           'cursor-sensor-functions `((lambda (&rest _) (message "Remove advice `%s'" ',advice)))
           'help-echo (format "Remove advice `%s'" advice)
           'action (lambda (_)
                     (when (yes-or-no-p (format "Remove advice `%s'?" advice))
                       (message "Removing advice `%s' from function `%s'" advice function)
                       (advice-remove function advice)
                       (if (eq major-mode 'helpful-mode)
                           (helpful-update)
                         (revert-buffer nil t))))
           'follow-link t))))

    (defun add-button-to-remove-advice (buffer-name function)
      "Add a button to remove advice."
      (when (get-buffer buffer-name)
        (with-current-buffer buffer-name
          (save-excursion
            (goto-char (point-min))
            (let ((ad-list (function-advices function)))
              (while (re-search-forward "^\\(?:This function has \\)?:[-a-z]+ advice: \\(.+\\)\\.$" nil t)
                (let* ((name (string-trim (match-string 1) "[‘'`]" "[’']"))
                       (advice (intern-soft name)))
                  (when (memq advice ad-list)
                    (add-remove-advice-button advice function)
                    (setq ad-list (delq advice ad-list)))))

              ;; Search `:around' advice
              (goto-char (point-min))
              (when (re-search-forward "^This function is advised.$" nil t)
                (add-remove-advice-button (car ad-list) function)))))))

    (define-advice describe-function-1 (:after (function) advice-remove-button)
      (add-button-to-remove-advice "*Help*" function))
    (with-eval-after-load 'helpful
      (define-advice helpful-update (:after () advice-remove-button)
        (when helpful--callable-p
          (add-button-to-remove-advice (helpful--buffer helpful--sym t) helpful--sym))))

    ;; Remove hooks
    (defun remove-hook-at-point ()
      "Remove the hook at the point in the *Help* buffer."
      (interactive)
      (unless (or (eq major-mode 'help-mode)
                  (eq major-mode 'helpful-mode)
                  (string= (buffer-name) "*Help*"))
        (error "Only for help-mode or helpful-mode"))
      (let ((orig-point (point)))
        (save-excursion
          (when-let
              ((hook (progn (goto-char (point-min)) (symbol-at-point)))
               (func (when (and
                            (or (re-search-forward (format "^Value:?[\s|\n]") nil t)
                                (goto-char orig-point))
                            (sexp-at-point))
                       (end-of-sexp)
                       (backward-char 1)
                       (catch 'break
                         (while t
                           (condition-case _err
                               (backward-sexp)
                             (scan-error (throw 'break nil)))
                           (let ((bounds (bounds-of-thing-at-point 'sexp)))
                             (when (<= (car bounds) orig-point (cdr bounds))
                               (throw 'break (sexp-at-point)))))))))
            (when (yes-or-no-p (format "Remove %s from %s? " func hook))
              (remove-hook hook func)
              (if (eq major-mode 'helpful-mode)
                  (helpful-update)
                (revert-buffer nil t)))))))
    (bind-key "r" #'remove-hook-at-point help-mode-map)))

;; Show function arglist or variable docstring
;; `global-eldoc-mode' is enabled by default.
(use-package eldoc
  :ensure nil
  :diminish)

;; Interactive macro expander
(use-package macrostep
  :custom-face
  (macrostep-expansion-highlight-face ((t (:inherit tooltip :extend t))))
  :bind (:map emacs-lisp-mode-map
              ("C-c e" . macrostep-expand)
              :map lisp-interaction-mode-map
              ("C-c e" . macrostep-expand)))

;; A better *Help* buffer
(use-package helpful
  :commands helpful--buffer
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-command] . helpful-command)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-key] . helpful-key)
         ([remap describe-symbol] . helpful-symbol)
         ("C-c C-d" . helpful-at-point)
         :map helpful-mode-map
         ("r" . remove-hook-at-point))
  :hook (helpful-mode . cursor-sensor-mode) ; for remove-advice button
  :init
  (with-no-warnings
    (with-eval-after-load 'counsel
      (setq counsel-describe-function-function #'helpful-callable
            counsel-describe-variable-function #'helpful-variable
            counsel-describe-symbol-function #'helpful-symbol
            counsel-descbinds-function #'helpful-callable))

    (with-eval-after-load 'apropos
      ;; patch apropos buttons to call helpful instead of help
      (dolist (fun-bt '(apropos-function apropos-macro apropos-command))
        (button-type-put
         fun-bt 'action
         (lambda (button)
           (helpful-callable (button-get button 'apropos-symbol)))))
      (dolist (var-bt '(apropos-variable apropos-user-option))
        (button-type-put
         var-bt 'action
         (lambda (button)
           (helpful-variable (button-get button 'apropos-symbol)))))))
  :config
  (with-no-warnings
    ;; Open the buffer in other window
    (defun my-helpful--navigate (button)
      "Navigate to the path this BUTTON represents."
      (find-file-other-window (substring-no-properties (button-get button 'path)))
      ;; We use `get-text-property' to work around an Emacs 25 bug:
      ;; http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=f7c4bad17d83297ee9a1b57552b1944020f23aea
      (-when-let (pos (get-text-property button 'position
                                         (marker-buffer button)))
        (helpful--goto-char-widen pos)))
    (advice-add #'helpful--navigate :override #'my-helpful--navigate)))

;; For ERT
(use-package overseer
  :diminish
  :hook (emacs-lisp-mode . overseer-mode))

;; C/C++ Mode
(use-package cc-mode
  :ensure nil
  :bind (:map c-mode-base-map
         ("C-c c" . compile))
  :hook (c-mode-common . (lambda () (c-set-style "stroustrup")))
  :init (setq-default c-basic-offset 4)
  :config
  (use-package modern-cpp-font-lock
    :diminish
    :init (modern-c++-font-lock-global-mode t)))
(use-package python
  :ensure nil
  :hook (inferior-python-mode . (lambda ()
                                  (process-query-on-exit-flag
                                   (get-process "Python"))))
  :init
  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil)
  :config
  ;; Default to Python 3. Prefer the versioned Python binaries since some
  ;; systems stupidly make the unversioned one point at Python 2.
  (when (and (executable-find "python3")
             (string= python-shell-interpreter "python"))
    (setq python-shell-interpreter "python3"))

  ;; Env vars
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-env "PYTHONPATH")))

(use-package markdown-mode)
(use-package lsp-bridge
  :ensure nil
  :defer 2
  :bind (:map lsp-bridge-mode
         ("C-M-n" . lsp-bridge-popup-documentation-scroll-up) ;向下滚动文档
         ("C-M-p" . lsp-bridge-popup-documentation-scroll-down) ;向上滚动文档
         )
  :load-path "~/github/lsp-bridge"
  :config
  ;; (setq lsp-bridge-python-command "/Users/albert/.virtualenvs/pandas/bin/python")
  (global-lsp-bridge-mode)
  ;; (setq lsp-bridge-enable-log t)
  (defun lsp-bridge-jump ()
    (interactive)
    (cond
     ((eq major-mode 'emacs-lisp-mode)
      (let ((symb (function-called-at-point)))
	(when symb
          (find-function symb))))
     (lsp-bridge-mode
      (lsp-bridge-find-def))
     (t
      (require 'dumb-jump)
      (dumb-jump-go))))

  (defun lsp-bridge-jump-back ()
    (interactive)
    (cond
     (lsp-bridge-mode
      (lsp-bridge-return-from-def))
     (t
      (require 'dumb-jump)
      (dump-jump-back))))
  )

;; 6. Utils
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
  :init
  (global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)
  (global-set-key (kbd "C-c l") 'evilnc-quick-comment-or-uncomment-to-the-line)
  (global-set-key (kbd "C-c k") 'evilnc-copy-and-comment-lines)
  (global-set-key (kbd "C-c m") 'evilnc-comment-or-uncomment-paragraphs))

(use-package rime
  :commands (toggle-input-method)
  :custom
  ;; (rime-librime-root "/usr/lib")
  (default-input-method 'rime)
  (rime-user-data-dir "~/.emacs.d/rime")
  (rime-show-candidate 'posframe)
  (rime-show-preedit t)
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
  (when (eq system-type 'windows-nt)
    (setq rime-librime-root "C:/msys64/mingw64/bin")
    )
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
          '(("\\.pdf$" "start" (file))))
    )
  :custom
  (openwith-mode t))

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
;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

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
        completion-category-overrides '((file (styles partial-completion)))))

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

;; 7. org
(use-package hydra)
(use-package pretty-hydra
  :bind ("<f6>" . toggles-hydra/body)
  :init
  (cl-defun pretty-hydra-title (title &optional icon-type icon-name
                                      &key face height v-adjust)
    "Add an icon in the hydra title."
    (let ((face (or face `(:foreground ,(face-background 'highlight))))
          (height (or height 1.0))
          (v-adjust (or v-adjust 0.0)))
      (concat
       (when (and (icon-displayable-p) icon-type icon-name)
         (let ((f (intern (format "all-the-icons-%s" icon-type))))
           (when (fboundp f)
             (concat
              (apply f (list icon-name :face face :height height :v-adjust v-adjust))
              " "))))
       (propertize title 'face face)))))

(require 'hydra)
(require 'pretty-hydra)
(use-package org
  :after pretty-hydra
  :ensure nil
  :commands (org-dynamic-block-define)
  :custom-face (org-ellipsis ((t (:foreground nil))))
  :pretty-hydra
  ((:title (pretty-hydra-title "Org Template" 'fileicon "org" :face 'all-the-icons-green :height 1.1 :v-adjust 0.0)
    :color blue :quit-key "q")
   ("Basic"
    (("a" (hot-expand "<a") "ascii")
     ("c" (hot-expand "<c") "center")
     ("C" (hot-expand "<C") "comment")
     ("e" (hot-expand "<e") "example")
     ("E" (hot-expand "<E") "export")
     ("h" (hot-expand "<h") "html")
     ("l" (hot-expand "<l") "latex")
     ("n" (hot-expand "<n") "note")
     ("o" (hot-expand "<q") "quote")
     ("v" (hot-expand "<v") "verse"))
    "Head"
    (("i" (hot-expand "<i") "index")
     ("A" (hot-expand "<A") "ASCII")
     ("I" (hot-expand "<I") "INCLUDE")
     ("H" (hot-expand "<H") "HTML")
     ("L" (hot-expand "<L") "LaTeX"))
    "Source"
    (("s" (hot-expand "<s") "src")
     ("m" (hot-expand "<s" "emacs-lisp") "emacs-lisp")
     ("y" (hot-expand "<s" "python :results output") "python")
     ("p" (hot-expand "<s" "perl") "perl")
     ("r" (hot-expand "<s" "ruby") "ruby")
     ("S" (hot-expand "<s" "sh") "sh")
     ("g" (hot-expand "<s" "go :imports '\(\"fmt\"\)") "golang"))
    "Misc"
    (("u" (hot-expand "<s" "plantuml :file CHANGE.png") "plantuml")
     ("Y" (hot-expand "<s" "ipython :session :exports both :results raw drawer\n$0") "ipython")
     ("P" (progn
            (insert "#+HEADERS: :results output :exports both :shebang \"#!/usr/bin/env perl\"\n")
            (hot-expand "<s" "perl")) "Perl tangled")
     ("<" self-insert-command "ins"))))
  :bind (("C-c a" . org-agenda)
         ("C-c b" . org-switchb)
         ("C-c x" . org-capture)
         :map org-mode-map
         ("<" . (lambda ()
                  "Insert org template."
                  (interactive)
                  (if (or (region-active-p) (looking-back "^\s*" 1))
                      (org-hydra/body)
                    (self-insert-command 1)))))
  :hook (((org-babel-after-execute org-mode) . org-redisplay-inline-images) ; display image
         (org-indent-mode . (lambda()
                              (diminish 'org-indent-mode)
                              ;; HACK: Prevent text moving around while using brackets
                              ;; @see https://github.com/seagle0128/.emacs.d/issues/88
                              (make-variable-buffer-local 'show-paren-mode)
                              (setq show-paren-mode nil))))
  :config
  (defun hot-expand (str &optional mod)
    "Expand org template.
STR is a structure template string recognised by org like <s. MOD is a
string with additional parameters to add the begin line of the
structure element. HEADER string includes more parameters that are
prepended to the element after the #+HEADER: tag."
    (let (text)
      (when (region-active-p)
        (setq text (buffer-substring (region-beginning) (region-end)))
        (delete-region (region-beginning) (region-end)))
      (insert str)
      (if (fboundp 'org-try-structure-completion)
          (org-try-structure-completion) ; < org 9
        (progn
          ;; New template expansion since org 9
          (require 'org-tempo nil t)
          (org-tempo-complete-tag)))
      (when mod (insert mod) (forward-line))
      (when text (insert text))))

  ;; Add :add-created property
  (defun org-current-is-todo ()
    (string= "TODO" (org-get-todo-state)))

  (add-hook 'org-capture-prepare-finalize-hook #'zp/org-capture-set-created-property)
  (defvar zp/org-created-property-name "CREATED"
    "The name of the org-mode property that stores the creation date of the entry")

  (defun zp/org-set-created-property (&optional active name)
    "Set a property on the entry giving the creation time.
By default the property is called CREATED. If given, the ‘NAME’
argument will be used instead. If the property already exists, it
will not be modified.
If the function sets CREATED, it returns its value."
    (interactive)
    (let* ((created (or name zp/org-created-property-name))
           (fmt (if active "<%s>" "[%s]"))
           (now (format fmt (format-time-string "%Y-%m-%d %a %H:%M"))))
      (unless (org-entry-get (point) created nil)
        (org-set-property created now)
        now)))

  (defun zp/org-capture-set-created-property ()
    "Conditionally set the CREATED property on captured trees."
    (let ((add-created (plist-get org-capture-plist :add-created))
          (type (plist-get org-capture-plist :type)))
      (when (and (eq type 'entry)
                 add-created)
        (unless (buffer-narrowed-p)
          (error "Buffer is not narrowed"))
        (save-excursion
          (goto-char (point-min))
          (zp/org-set-created-property)))))

  ;; agenda 显示农历
  ;; https://emacs-china.org/t/05-org-as/12092/31
  (setq org-agenda-format-date 'yuchen/org-agenda-format-date-aligned)

  (defun yuchen/org-agenda-format-date-aligned (date)
    "Format a DATE string for display in the daily/weekly agenda, or timeline.
      This function makes sure that dates are aligned for easy reading."
    (require 'cal-iso)
    (let* ((dayname (aref cal-china-x-days
                          (calendar-day-of-week date)))
           (day (cadr date))
           (month (car date))
           (year (nth 2 date))
           (cn-date (calendar-chinese-from-absolute (calendar-absolute-from-gregorian date)))
           (cn-month (cl-caddr cn-date))
           (cn-day (cl-cadddr cn-date))
           (cn-month-string (concat (aref cal-china-x-month-name
                                          (1- (floor cn-month)))
                                    (if (integerp cn-month)
                                        ""
                                      "(闰月)")))
           (cn-day-string (aref cal-china-x-day-name
                                (1- cn-day))))
      (format "%04d-%02d-%02d 周%s %s%s" year month
              day dayname cn-month-string cn-day-string)))

  ;; To speed up startup, don't put to init section
  (setq org-modules nil                 ; Faster loading
        org-directory own-org-directory
        org-capture-templates
        `(("i" "Idea" entry (file ,(concat own-org-directory "/idea.org"))
           "*  %^{Title} %?\n%U\n%a\n")
          ("t" "Todo" entry (file ,(concat own-org-directory "/inbox.org"))
           "* TODO %?\n%a\n" :add-created t)
          )
        org-todo-keywords
        '((sequence "BUG(b!)" "|" "FIXED(f!)")
          (sequence "TODO(t!)" "SOMEDAY(s)" "|" "DONE(d!)" "CANCELED(c @/!)"))

        org-priority-faces '((?A . error)
                             (?B . warning)
                             (?C . success))

        org-complete-tags-always-offer-all-agenda-tags t

        org-tag-persistent-alist '(("Reminder" .?r)
                                   ("THIS_WEEK" . ?t)
                                   (:startgrouptag)
                                   ("CONTEXT")
                                   (:grouptags)
                                   ("@Sicnu" . ?s)
                                   ("@SWJTU" . ?j)
                                   ("@Home" . ?h)
                                   ("@BUS" . ?b)
                                   ("@南充" . ?n)
                                   ("@出差" . ?B)
                                   (:endgrouptag)
                                   (:startgrouptag)
                                   ("TOOLS")
                                   (:grouptags)
                                   ("Phone" . ?p)
                                   ("Mac" . ?m)
                                   ("Windows" . ?w)
                                   ("Pad" . ?P)
                                   (:endgrouptag)
                                   (:startgroup)
                                   ("POMO")
                                   (:grouptags)
                                   ("IN" . ?i)
                                   ("OUT" . ?o)
                                   (:endgroup))

        ;; Agenda styling
        org-agenda-files `(,own-org-directory)
        org-archive-location "%s.archive::"
        org-agenda-block-separator ?─
        org-agenda-time-grid
        '((daily today require-timed)
          (800 1000 1200 1400 1600 1800 2000)
          " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
        org-agenda-current-time-string
        "⭠ now ─────────────────────────────────────────────────"

        org-tags-column -80
        org-log-done 'time
        org-catch-invisible-edits 'smart
        org-startup-indented t
        org-ellipsis (if (char-displayable-p ?⏷) "\t⏷" nil)
        org-pretty-entities nil
        org-hide-emphasis-markers t
        org-refile-files (append (file-expand-wildcards (concat own-org-directory "/*.org"))
                                 (file-expand-wildcards (concat own-org-directory "/daily/*.org")))
        org-refile-targets
        '((org-refile-files . (:maxlevel . 2))
          (nil . (:level . 1)))
        org-agenda-clockreport-parameter-plist '(:scope agenda-with-archives :link t :maxlevel 3)
        org-agenda-archives-mode t)

  (defadvice org-archive-subtree (around fix-hierarchy activate)
    (let* ((fix-archive-p (and (not current-prefix-arg)
                               (not (use-region-p))))
           (location (org-archive--compute-location
                      (or (org-entry-get nil "ARCHIVE" 'inherit)
                          org-archive-location)))
           (afile (car location))
           (buffer (or (find-buffer-visiting afile) (find-file-noselect afile))))
      ad-do-it
      (when fix-archive-p
        (with-current-buffer buffer
          (goto-char (point-max))
          (while (org-up-heading-safe))
          (let* ((olpath (org-entry-get (point) "ARCHIVE_OLPATH"))
                 (path (and olpath (split-string olpath "/")))
                 (level 1)
                 tree-text)
            (when olpath
              (org-mark-subtree)
              (setq tree-text (buffer-substring (region-beginning) (region-end)))
              (let (this-command) (org-cut-subtree))
              (goto-char (point-min))
              (save-restriction
                (widen)
                (-each path
                  (lambda (heading)
                    (if (re-search-forward
                         (rx-to-string
                          `(: bol (repeat ,level "*") (1+ " ") ,heading)) nil t)
                        (org-narrow-to-subtree)
                      (goto-char (point-max))
                      (unless (looking-at "^")
                        (insert "\n"))
                      (insert (make-string level ?*)
                              " "
                              heading
                              "\n"))
                    (cl-incf level)))
                (widen)
                (org-end-of-subtree t t)
                (org-paste-subtree level tree-text))))))))

  ;; Add new template
  (add-to-list 'org-structure-template-alist '("n" . "note"))

  ;; Use embedded webkit browser if possible
  (when (featurep 'xwidget-internal)
    (push '("\\.\\(x?html?\\|pdf\\)\\'"
            .
            (lambda (file _link)
              (xwidget-webkit-browse-url (concat "file://" file))
              (let ((buf (xwidget-buffer (xwidget-webkit-current-session))))
                (when (buffer-live-p buf)
                  (and (eq buf (current-buffer)) (quit-window))
                  (pop-to-buffer buf)))))
          org-file-apps))

  ;; Add gfm/md backends
  (use-package ox-gfm)
  (add-to-list 'org-export-backends 'md)

  (with-eval-after-load 'counsel
    (bind-key [remap org-set-tags-command] #'counsel-org-tag org-mode-map))

  ;; Prettify UI

  (use-package org-modern
    :hook ((org-mode . org-modern-mode)
           (org-agenda-finalize . org-modern-agenda)
           (org-modern-mode . (lambda ()
                                "Adapt `org-modern-mode'."
                                ;; Disable Prettify Symbols mode
                                (setq prettify-symbols-alist nil)
                                (prettify-symbols-mode -1)))))
  (progn
    (use-package org-superstar
      :if (and (display-graphic-p) (char-displayable-p ?◉))
      :hook (org-mode . org-superstar-mode)
      :init (setq org-superstar-headline-bullets-list '("◉""○""◈""◇""⁕")))
    (use-package org-fancy-priorities
      :diminish
      :hook (org-mode . org-fancy-priorities-mode)
      :init (setq org-fancy-priorities-list
                  (if (and (display-graphic-p) (char-displayable-p ?🅐))
                      '("🅐" "🅑" "🅒" "🅓")
                    '("HIGH" "MEDIUM" "LOW" "OPTIONAL")))))

  ;; Babel
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t)

  (defvar load-language-list '((emacs-lisp . t)
                               (perl . t)
                               (python . t)
                               (ruby . t)
                               (js . t)
                               (css . t)
                               (sass . t)
                               (C . t)
                               (java . t)
                               (plantuml . t)))

  ;; ob-sh renamed to ob-shell since 26.1.
  (cl-pushnew '(shell . t) load-language-list)

  (use-package ob-go
    :init (cl-pushnew '(go . t) load-language-list))

  (use-package ob-rust
    :init (cl-pushnew '(rust . t) load-language-list))

  ;; Use mermadi-cli: npm install -g @mermaid-js/mermaid-cli
  (use-package ob-mermaid
    :init (cl-pushnew '(mermaid . t) load-language-list))

  (org-babel-do-load-languages 'org-babel-load-languages
                               load-language-list)

  ;; Rich text clipboard
  (use-package org-rich-yank
    :bind (:map org-mode-map
	   ("C-M-y" . org-rich-yank)))

  ;; Table of contents
  (use-package toc-org
    :hook (org-mode . toc-org-mode))

  ;; Export text/html MIME emails
  (use-package org-mime
    :bind (:map message-mode-map
	   ("C-c M-o" . org-mime-htmlize)
	   :map org-mode-map
	   ("C-c M-o" . org-mime-org-buffer-htmlize)))

  ;; Add graphical view of agenda
  (use-package org-timeline
    :hook (org-agenda-finalize . org-timeline-insert-timeline))

  ;; Auto-toggle Org LaTeX fragments
  (use-package org-fragtog
    :diminish
    :hook (org-mode . org-fragtog-mode))

  ;; Preview
  (use-package org-preview-html
    :diminish
    :bind (:map org-mode-map
	   ("C-c C-h" . org-preview-html-mode))
    :init (when (featurep 'xwidget-internal)
            (setq org-preview-html-viewer 'xwidget)))

  ;; Presentation
  (use-package org-tree-slide
    :diminish
    :functions (org-display-inline-images
                org-remove-inline-images)
    :bind (:map org-mode-map
	   ("s-<f7>" . org-tree-slide-mode)
	   :map org-tree-slide-mode-map
	   ("<left>" . org-tree-slide-move-previous-tree)
	   ("<right>" . org-tree-slide-move-next-tree)
	   ("S-SPC" . org-tree-slide-move-previous-tree)
	   ("SPC" . org-tree-slide-move-next-tree))
    :hook ((org-tree-slide-play . (lambda ()
                                    (text-scale-increase 4)
                                    (org-display-inline-images)
                                    (read-only-mode 1)))
           (org-tree-slide-stop . (lambda ()
                                    (text-scale-increase 0)
                                    (org-remove-inline-images)
                                    (read-only-mode -1))))
    :init (setq org-tree-slide-header nil
                org-tree-slide-slide-in-effect t
                org-tree-slide-heading-emphasis nil
                org-tree-slide-cursor-init t
                org-tree-slide-modeline-display 'outside
                org-tree-slide-skip-done nil
                org-tree-slide-skip-comments t
                org-tree-slide-skip-outline-level 3))

  (use-package vulpea
    :init
    (add-to-list 'org-tags-exclude-from-inheritance "project")
    :custom
    (defun vulpea-project-p ()
      "Return non-nil if current buffer has any todo entry.
TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks."
      (seq-find                                 ; (3)
       (lambda (type)
         (eq type 'todo))
       (org-element-map                         ; (2)
           (org-element-parse-buffer 'headline) ; (1)
           'headline
         (lambda (h)
           (org-element-property :todo-type h)))))

    (defun vulpea-project-update-tag ()
      "Update PROJECT tag in the current buffer."
      (when (and (not (active-minibuffer-window))
                 (vulpea-buffer-p))
        (save-excursion
          (goto-char (point-min))
          (let* ((tags (vulpea-buffer-tags-get))
                 (original-tags tags))
            (if (vulpea-project-p)
                (setq tags (cons "project" tags))
              (setq tags (remove "project" tags)))

            ;; cleanup duplicates
            (setq tags (seq-uniq tags))

            ;; update tags if changed
            (when (or (seq-difference tags original-tags)
                      (seq-difference original-tags tags))
              (apply #'vulpea-buffer-tags-set tags))))))

    (defun vulpea-buffer-p ()
      "Return non-nil if the currently visited buffer is a note."
      (and buffer-file-name
           (string-prefix-p
            (expand-file-name (file-name-as-directory own-org-directory))
            (file-name-directory buffer-file-name))))

    (defun vulpea-project-files ()
      "Return a list of note files containing 'project' tag." ;
      (seq-uniq
       (seq-map
        #'car
        (org-roam-db-query
         [:select [nodes:file]
	  :from tags
	  :left-join nodes
	  :on (= tags:node-id nodes:id)
	  :where (like tag (quote "%\"project\"%"))]))))
    (defun vulpea-agenda-files-update (&rest _)
      "Update the value of `org-agenda-files'."
      (setq org-agenda-files (vulpea-project-files)))
    (add-hook 'find-file-hook #'vulpea-project-update-tag)
    (add-hook 'before-save-hook #'vulpea-project-update-tag)
    (advice-add 'org-agenda-files :filter-return #'vulpea-agenda-files-update)
    )
  (use-package org-appear
    :ensure t
    :init
    (add-hook 'org-mode-hook 'org-appear-mode)
    (setq org-hide-emphasis-markers t)
    :custom
    (setq org-appear-autolinks t))
  ;; org-download screen shot
  (use-package org-download
    :ensure t
    :init
    (add-hook 'dired-mode-hook 'org-download-enable)
    (with-eval-after-load 'org
      (org-download-enable))
    :config
    (setq org-download-image-dir (concat own-org-directory "/images"))
    (defun dummy-org-download-annotate-function (link)
      "")
    (if (eq system-type 'darwin)
        (setq org-download-screenshot-method "screencapture -i %s")
      )
    (if (eq system-type 'windows-nt)
        (setq org-download-screenshot-method "c:/Progra~1/IrfanView/i_view64.exe /capture=4 /convert=\"%s\"")
      )
    (setq org-download-annotate-function
          #'dummy-org-download-annotate-function)
    :custom
    (org-image-actual-width 360)
    (org-download-heading-lvl nil)
    (org-download-image-dir (concat own-org-directory "/images"))
    )

  ;; Pomodoro
  (use-package org-pomodoro
    :custom-face
    (org-pomodoro-mode-line ((t (:inherit warning))))
    (org-pomodoro-mode-line-overtime ((t (:inherit error))))
    (org-pomodoro-mode-line-break ((t (:inherit success))))
    :bind (:map org-mode-map
	   ("C-c C-x m" . org-pomodoro))
    :init
    (with-eval-after-load 'org-agenda
      (bind-keys :map org-agenda-mode-map
	("K" . org-pomodoro)
	("C-c C-x m" . org-pomodoro)))
    :custom
    ;; 设置org-pomodoro桌面全局通知
    ;; 定义通知函数
    ;; (defun notify-osx (title message)
    ;;   (call-process "terminal-notifier"
    ;; 		        nil 0 nil
    ;; 		        "-group" "Emacs"
    ;; 		        "-title" title
    ;; 		        "-sender" "org.gnu.Emacs"
    ;; 		        "-message" message
    ;; 		        "-activate" "org.gnu.Emacs"))

    (defun notify-osx (title message)
      (call-process "notify-send"
		    message
		    ))
    (add-hook 'org-pomodoro-finished-hook
	      (lambda ()
		(notify-osx "Org-pomodoro GTD 完成" "休息5分钟")))
    (add-hook 'org-pomodoro-break-finished-hook
	      (lambda ()
                (notify-osx "Org-pomodoro GTD 休息完成" "设置开始下一个？")))
    (add-hook 'org-pomodoro-long-break-finished-hook
	      (lambda ()
		(notify-osx "Org-pomodoro GTD 长休息完成" "设置开始下一个？")))
    (add-hook 'org-pomodoro-killed-hook
	      (lambda ()
		(notify-osx "Org-pomodoro GTD 番茄取消" "取消！！！")))
    )
  (use-package org-super-agenda
    :ensure t
    :after org-agenda
    :init
    (setq org-agenda-skip-scheduled-if-done t
          org-agenda-skip-deadline-if-done t
          org-agenda-compact-blocks t
          org-agenda-start-day "+0d"
          org-agenda-span 1)
    (setq org-agenda-custom-commands
          '(("p" "Project View"
             ((alltodo "" ((org-agenda-override-header "")
                           (org-super-agenda-groups
                            '((:name "Project View"
			       :auto-parent t))
                            )))))
            ("P" "This week"
             ((alltodo "" ((org-agenda-override-header "")
                           (org-super-agenda-groups
                            '((:name "This Weak"
			       :and (:todo "TODO" :tag "THIS_WEEK")))
                            )))))
            ("z" "Super view"
             ((agenda "" ((org-agenda-span 'day)
                          (org-super-agenda-groups
                           '((:name "Today"
			      :time-grid t
			      :date today
			      :todo "TODO"
			      :scheduled today
			      :order 1)))))
              (alltodo "" ((org-agenda-overriding-header "")
                           (org-super-agenda-groups
                            '((:name "Next @Sicnu"
			       :and (:todo "TODO" :tag "THIS_WEEK" :tag "@Sicnu" :priority>="B")
			       :order 1)
                              (:name "Next @Home"
			       :and (:tag "THIS_WEEK" :tag "@Home" :todo "TODO" :priority>="B")
			       :order 2)
                              (:name "Important"
			       :and (:todo "TODO" :priority>="B")
			       :order 3)
                              (:name "Due Today"
			       :deadline today
			       :order 4)
                              (:name "Due Soon"
			       :deadline future
			       :order 8)
                              (:name "Overdue"
			       :deadline past
			       :order 7)
                              (:name "This Week"
			       :tag "THIS_WEEK"
			       :order 5)
                              (:name "At SICNU"
			       :tag "@Sicnu"
			       :order 13)
                              (:name "At SWJTU"
			       :tag "@SWJTU"
			       :order 13)
                              (:name "trivial"
			       :priority<= "C"
			       :tag ("Trivial" "Unimportant")
			       :todo ("SOMEDAY")
			       :order 90)
                              (:discard (:tag ("Chore" "Routine" "Daily")))))))))
            ("r" . "Review")
            ("rw" "Review Archives"
             tags "closed>=\"<-1w>\"")
            ("rd" "Review Day"
             tags "closed>=\"<today>\"")
            ("rm" "Review month"
             tags "closed>=\"<-1m>\"")
            ("f" "查看TODO条目（按创建时间排序）" todo "TODO"
             ((org-agenda-sorting-strategy '(priority-down time-up))))
            )
          )
    :custom
    (org-super-agenda-mode t)))

(use-package deft
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-extensions '("org"))
  (deft-directory own-org-directory)
  (add-hook 'deft-mode-hook (lambda() (display-line-numbers-mode -1)))
  (add-hook 'org-mode-hook (lambda () (electric-indent-local-mode -1)))
  (deft-recursive-ignore-dir-regexp
    (concat "\\(?:"
            "\\."
            "\\|\\.\\."
            "\\\|common"
            "\\|daily"
            "\\|archive_notes"
            "\\|auto"
            "\\|_minted.*"
            "\\)$"))
  :config
  (defun cm/deft-parse-title (file contents)
    "Parse the given FILE and CONTENTS and determine the title.
  If `deft-use-filename-as-title' is nil, the title is taken to
  be the first non-empty line of the FILE.  Else the base name of the FILE is
  used as title."
    (let ((begin (string-match "^#\\+[tT][iI][tT][lL][eE]: .*$" contents)))
      (if begin
	  (string-trim (substring contents begin (match-end 0)) "#\\+[tT][iI][tT][lL][eE]: *" "[\n\t ]+")
	(deft-base-filename file))))

  (advice-add 'deft-parse-title :override #'cm/deft-parse-title)

  (setq deft-strip-summary-regexp
	(concat "\\("
		"[\n\t]" ;; blank
		"\\|^#\\+[[:alpha:]_]+:.*$" ;; org-mode metadata
		"\\|^:PROPERTIES:\n\\(.+\n\\)+:END:\n"
		"\\)")))
;; Roam
(use-package emacsql-sqlite-builtin)
(use-package org-roam
  :diminish
  :hook (after-init . org-roam-db-autosync-enable)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n j" . org-roam-dailies-capture-today))
  :init
  (setq org-roam-directory (file-truename own-org-directory))
  ;; (setq org-roam-v2-ack t)
  :config
  (unless (file-exists-p org-roam-directory)
    (make-directory org-roam-directory))
  (org-roam-db-autosync-enable)

  (use-package org-roam-ui
    :init
    (when (featurep 'xwidget-internal)
      (setq org-roam-ui-browser-function #'xwidget-webkit-browse-url)))
  
  :custom

  (org-roam-database-connector 'sqlite-builtin)
  (org-roam-db-gc-threshold most-positive-fixnum)
  (defun org-roam-open-refs ()
    "Open REFs of the node at point."
    (interactive)
    (save-excursion
      (goto-char (org-roam-node-point (org-roam-node-at-point 'assert)))
      (when-let* ((p (org-entry-get (point) "ROAM_REFS"))
                  (refs (when p (split-string-and-unquote p)))
                  (refs (if (length> refs 1)
                            (completing-read-multiple "Open: " refs)
                          refs))
                  (oc-cites
                   (seq-map
                    (lambda (ref) (substring ref 1))
                    (seq-filter (apply-partially #'string-prefix-p "@") refs)))
                  (user-error "No ROAM_REFS found"))

        (citar-run-default-action oc-cites)))))

;; 8. paper
;; config the org-roam-bibtex
(use-package org-ref
  :after org)
(use-package org-roam-bibtex
  :after org-roam
  :init
  (org-roam-bibtex-mode 1)
  :custom
  (orb-note-actions-interface 'default)
  :config
  (require 'org-ref)) ; optional: if using Org-ref v2 or v3 citation links


(use-package bibtex
  :defer t
  :config
  (setq bibtex-file-path (concat own-org-directory "references/")
        bibtex-files '("NSF_Fund.bib")
        bibtex-notes-path (concat own-org-directory "references/notes")
        bibtex-align-at-equal-sign t
        bibtex-autokey-titleword-separator "-"
        bibtex-autokey-year-title-separator "-"
        bibtex-autokey-name-year-separator "-"
        bibtex-dialect 'biblatex))

(use-package embark
  :ensure t)

(use-package citeproc
  :ensure t
  :config
  (require 'oc-basic)
  (require 'oc-csl)
  (require 'oc-natbib)
  (require 'oc-biblatex)
  )

(use-package citar
  :init
  (setq org-cite-insert-processor 'citar
        org-cite-follow-processor 'citar
        org-cite-activate-processor 'citar
        citar-citeproc-csl-styles-dir (concat own-org-directory "references/")
        citar-citeproc-csl-locales-dir (concat own-org-directory "references/locals")
        bibtex-file-path (concat own-org-directory "references/")
        citar-format-reference-function #'citar-citeproc-format-reference
        citar-file-open-prompt nil)
  :config
  (setq citar-at-point-function 'embark-act
        citar-bibliography (mapcar (lambda (file) (concat bibtex-file-path file)) bibtex-files)
        citar-library-paths `(,(concat bibtex-file-path "papers/"))
        citar-notes-paths `(,bibtex-notes-path)))
;; ref: https://github.com/bdarcus/citar/issues/531


(use-package ebib
  ;; :commands ebib-zotero-protocol-handler
  :init
  ;; (with-eval-after-load 'org-protocol
  ;;   (push '("ebib-zotero" :protocol "ebib-zotero" :function ebib-zotero-protocol-handler)
  ;;         org-protocol-protocol-alist))
  (setq ebib-default-directory bibtex-file-path)
  :bind
  ("C-c n b" . ebib)
  :custom
  (bibtex-autokey-name-case-convert-function 'capitalize)
  (bibtex-autokey-titlewords 0)
  (bibtex-autokey-year-length 4)
  (ebib-uniquify-keys t)
  (ebib-bibtex-dialect 'biblatex)
  (ebib-index-window-size 10)
  (ebib-file-search-dirs `(,(concat bibtex-file-path "papers")))
  (ebib-reading-list-file (concat bibtex-file-path "../reading_list.org"))
  (ebib-keywords-field-keep-sorted t)
  (ebib-use-timestamp t)
  (ebib-filters-default-file (concat bibtex-file-path "ebib-filters"))
  (ebib-file-associations '(("pdf")))
  (ebib-index-columns '(("Entry Key" 20 t)
			("Author/Editor" 40 nil)
			("Year" 6 t)
			("Title" 50 t)))
  (ebib-notes-storage 'multiple-notes-per-file)
  (ebib-index-default-sort '("timestamp" . descend))
  :config
  (setq ebib-notes-template "* %T\n:PROPERTIES:\n%K\n:ROAM_REFS: @%k\n:ID:  %i\n:NOTER_DOCUMENT: %F\n:END:\n%%?\n[cite:@%k]\nDate: %S\n"
        ebib-notes-default-file (concat bibtex-file-path "../Ref_notes.org")
        ebib-keywords (concat bibtex-file-path "keywords.txt")
        ebib-preload-bib-files `(,(concat bibtex-file-path "NSF_Fund.bib"))
        ebib-notes-template-specifiers '((?k . ebib-create-key)
                                         (?i . ebib-create-id)
                                         (?K . ebib-create-org-identifier)
                                         (?X . ebib-create-org-title)
    		                         (?T . ebib-create-org-description)
                                         (?L . ebib-create-org-link)
                                         (?F . ebib-create-org-file-name)
                                         (?S . ebib-create-org-time-stamp)))
  (defun ebib-create-org-file-name (key db)
    "Create an org link to the file in entry KEY in DB.
The file is taken from the \"file\" filed in the entry designated
by KEY in the current database.  If that field contains more than
one file name, the user is asked to select one.  If
the \"file\" field is empty, return the empty string."
    (let ((files (ebib-get-field-value "file" key db 'noerror 'unbraced 'xref)))
      (if files
          (let* ((absolute-path (ebib--expand-file-name (ebib--select-file files nil key)))
                 (relative-path (file-relative-name absolute-path default-directory))
                 (abbreviate-path (abbreviate-file-name absolute-path))
                 (final-path
                  (cl-case ebib-link-file-path-type
                    (relative relative-path)
                    (adaptive (if (string-match (concat "^" (regexp-quote default-directory))
                                                absolute-path)
                                  relative-path
                                abbreviate-path))
                    (otherwise absolute-path))))
            (format "%s" final-path))
        "")))
  (defun ebib-create-key (key _db)
    "Return the KEY in DB for the Org mode note."
    (format "%s" key))

  (defun ebib-create-id (_key _db)
    "Create an ID for the Org mode note."
    (org-id-new))

  (defun ebib-create-org-time-stamp (_key _db)
    "Create timestamp for the Org mode note."
    (format "%s" (with-temp-buffer (org-insert-time-stamp nil)))))

(setq org-latex-pdf-process
      '("xelatex -interaction nonstopmode -output-directory %o %f"
        "biber --output-directory %o $(basename %f .tex)"
        "xelatex -interaction nonstopmode -output-directory %o %f"
        "xelatex -interaction nonstopmode -output-directory %o %f")
      )

(load-theme 'doom-solarized-light 'no-confirm)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
