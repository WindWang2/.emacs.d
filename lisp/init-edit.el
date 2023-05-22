;; init-edit.el --- Initialize version control system configurations.	-*- lexical-binding: t -*-

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
;; edit.
;;
(defun org-english-writing-font ()
  "Sets a fixed width (monospace) font in current buffer"
  (interactive)
  (setq buffer-face-mode-face '(:family "Times New Roman" :height 120))
  (when sys/macp
    (setq buffer-face-mode-face '(:family "Times New Roman" :height 150)))
  (buffer-face-mode))

(use-package autorevert
  :ensure nil
  :defer t
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
  :defer t
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
  :defer t
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
         (org-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)
         (flyspell-mode . (lambda ()
                            (dolist (key '("C-;" "C-," "C-."))
                              (unbind-key key flyspell-mode-map)))))
  :init (setq flyspell-issue-message-flag nil
              ispell-program-name "enchant-2"
              ispell-dictionary "english")
  ;; ispell-program-name "aspell"
  ;; ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together"))
  :config
  ;; (add-hook 'org-mode-hook (lambda () (flyspell-mode -1)))
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
  :defer t
  :hook (after-init . yas-global-mode))

(use-package yasnippet-snippets
  :defer t
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
  (use-package conda
    :init
    (setq conda-anaconda-home "/opt/miniconda3")
    (when sys/linuxp
      (if (file-exists-p "/opt/anaconda")
          (setq conda-anaconda-home "/opt/anaconda")
        (setq conda-anaconda-home "/opt/miniconda3")))
    (when sys/macp
      (setq conda-anaconda-home (expand-file-name "~/mambaforge/")))

    :config
    (conda-env-initialize-interactive-shells)
    (conda-env-initialize-eshell)
    ;; (conda-env-autoactivate-mode t)
    (add-hook 'find-file-hook (lambda () (when (bound-and-true-p conda-project-env-path)
                                           (conda-env-activate-for-buffer))))
    )

  ;; (when (and (executable-find "python3")
  ;;            (string= python-shell-interpreter "python"))
  ;;   (setq python-shell-interpreter "python3"))
  ;; (when sys/macp
  ;;   (setq python-shell-interpreter (expand-file-name "~/mambaforge/bin/python3")))
  ;; (when sys/linuxp
  ;;   (setq python-shell-interpreter "/opt/miniconda3/bin/python3"))
  ;; Env vars
  ;; (with-eval-after-load 'exec-path-from-shell
  ;;   (exec-path-from-shell-copy-env "PYTHONPATH"))
  )

(use-package treesit-auto
  :hook (after-init . global-treesit-auto-mode)
  :init (setq treesit-auto-install 'prompt))

(use-package markdown-mode
  :defer t)

(when (display-graphic-p)
  (add-to-list 'load-path "~/github/lsp-bridge")
  (require 'yasnippet)
  (yas-global-mode 1)
  (require 'lsp-bridge)
  (global-lsp-bridge-mode)
  (add-to-list 'load-path "~/github/blink-search")
  (require 'blink-search)
  (when sys/macp
    (setq lsp-bridge-python-command (expand-file-name "~/mambaforge/bin/python"))
    (setq lsp-bridge-python-multi-lsp-server "pyright_ruff"))
  (add-hook 'python-mode-hook (lambda () (conda-env-activate "base")))
  (add-hook 'conda-postactivate-hook
            (lambda ()
              (lsp-bridge-restart-process)))
  )

(use-package pangu-spacing)
(provide 'init-edit)
