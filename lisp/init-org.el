;; init-org.el --- Initialize version control system configurations.	-*- lexical-binding: t -*-

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
;; org config files
;;

;; 7. org

(use-package org
  :after pretty-hydra
  :ensure nil
  :commands (org-dynamic-block-define)
  :custom-face (org-ellipsis ((t (:foreground unspecified))))
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
        `(("i" "Idea" entry (file ,(concat own-org-directory "/tasks/idea.org"))
           "*  %^{Title} %?\n%U\n%a\n")
          ("t" "Todo" entry (file ,(concat own-org-directory "/tasks/inbox.org"))
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
        ;; org-agenda-files (directory-files-recursively own-org-directory "\\.org$")
        ;; org-agenda-files (seq-filter (lambda(x) (not (string-match "/Archive/"(file-name-directory x))))
        ;;                              (directory-files-recursively own-org-directory "\\.org$"))

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
        org-refile-files (append (file-expand-wildcards (concat own-org-directory "notes/*.org"))
                                 (file-expand-wildcards (concat own-org-directory "tasks/*.org"))
                                 (file-expand-wildcards (concat own-org-directory "project/*.org")))
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
    :defer t
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
      :defer t
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
    )
  ;; :hook (org-mode . org-fragtog-mode))
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


  (use-package org-appear
    :defer t
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

  ;; (use-package org-wild-notifier)
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
    :config
    (when sys/win32p
      (defun notify-pomo (title message)
        "Temporary replacement for function of the same name which uses the buggy alert.el package. TITLE is the title of the MESSAGE."
        (let*
            ((toast "toast")
             (t-title (concat " -t \"" title))
             (t-message (concat "\" -m \"" message "\""))
             (t-image (concat " -p \"C:\\emacs-app\\share\\icons\\hicolor\\128x128\\apps\\emacs.png\""))
             (my-command (concat toast t-title t-message t-image)))
          (call-process-shell-command my-command))))
    (when sys/macp
      (defun notify-pomo (title message)
        (call-process "terminal-notifier"
                      nil 0 nil
                      "-group" "Emacs"
                      "-title" title
                      "-sender" "org.gnu.Emacs"
                      "-message" message
                      "-activate" "org.gnu.Emacs")))
    (when sys/linuxp
      (defun notify-pomo (title message)
        (call-process "notify-send"
		              message
		              )))

    (add-hook 'org-pomodoro-finished-hook
	          (lambda ()
		        (notify-pomo "Org-pomodoro GTD 完成" "休息5分钟")))
    (add-hook 'org-pomodoro-break-finished-hook
	          (lambda ()
                (notify-pomo "Org-pomodoro GTD 休息完成" "设置开始下一个？")))
    (add-hook 'org-pomodoro-long-break-finished-hook
	          (lambda ()
		        (notify-pomo "Org-pomodoro GTD 长休息完成" "设置开始下一个？")))
    (add-hook 'org-pomodoro-killed-hook
	          (lambda ()
		        (notify-pomo "Org-pomodoro GTD 番茄取消" "取消！！！")))
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
			                   :and (:todo "TODO" :tag ("@Sicnu" "THIS_WEEK") :priority>="B")
			                   :order 1)
                              (:name "Next @Home"
			                   :and (:tag ("THIS_WEEK" "@Home") :todo "TODO" :priority>="B")
			                   :order 2)
                              (:name "Important"
			                   :and (:todo "TODO" :priority>="B")
			                   :order 3)
                              (:name "Not Important"
			                   :and (:todo "TODO" :priority>="C")
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
                              (:name "Reading List"
			                   :and (:file-path "reading_list" :todo "TODO")
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

(use-package org-contrib
  :pin nongnu
  )

(use-package deft
  :defer t
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
            "\\|archive"
            "\\|collections"
            "\\|auto"
            "\\|paper_notes"
            "\\|_minted.*"
            "\\)$"))
  :config
  (setq deft-incremental-search nil)
  (defun deft-search-forward (str)
    "Function to use when matching files against filter strings STR.
This function calls `search-forward' when `deft-incremental-search'
is non-nil and `re-search-forward' otherwise."
    (let ((case-fold-search deft-case-fold-search))
      (if deft-incremental-search
          (search-forward str nil t)
        (re-search-forward (pinyinlib-build-regexp-string str) nil t))))
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
;; (use-package emacsql-sqlite
;;   :ensure t)
;; (use-package emacsql-sqlite-builtin)
(use-package org-roam
  ;; :after emacsql-sqlite-builtin
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
  (setq org-roam-database-connector 'sqlite-builtin)
  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
        (file-name-nondirectory
         (directory-file-name
          (file-name-directory
           (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (error "")))
  (unless (file-exists-p org-roam-directory)
    (make-directory org-roam-directory))
  (org-roam-db-autosync-enable)
  (use-package org-roam-ui
    :init
    (when (featurep 'xwidget-internal)
      (setq org-roam-ui-browser-function #'xwidget-webkit-browse-url))
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-follow-mode t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

  :custom
  (org-roam-db-gc-threshold most-positive-fixnum)
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates '(("d" "default (main)" plain "%?" :target
                                 (file+head "%<%Y>-${slug}.org" "#+title: ${title}\n")
                                 :imediate-finish t
                                 :unnarrowed t)
                                ("k" "knowledge (collection)" plain "%?" :target
                                 (file+head "collections/${title}.org" "#+title: ${title}\n")
                                 :imediate-finish t
                                 :unnarrowed t)
                                ("p" "Project (project)" plain "%?" :target
                                 (file+head "project/${title}.org" "#+title: Project: ${title}\n")
                                 :imediate-finish t
                                 :unnarrowed t)
                                ))
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
        (citar-run-default-action oc-cites))))

  (org-roam-node-display-template
   (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag))))
(use-package emacsql-sqlite-builtin)
(use-package vulpea
  :ensure t
  :after org-roam
  :hook (org-roam-db-autosync-mode . vulpea-db-autosync-enable)
  :init
  (add-to-list 'org-tags-exclude-from-inheritance "project")
  :config
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
    "Return non-nil if the currently visited buffer is a note with the addr 'org'."
    (and (string-equal "org" (file-name-extension buffer-file-name))
         (and buffer-file-name
              (string-prefix-p
               (expand-file-name (file-name-as-directory own-org-directory))
               (file-name-directory buffer-file-name)))))

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

  ;; functions borrowed from `vulpea' library
  ;; https://github.com/d12frosted/vulpea/blob/6a735c34f1f64e1f70da77989e9ce8da7864e5ff/vulpea-buffer.el

  (defun vulpea-buffer-tags-get ()
    "Return filetags value in current buffer."
    (vulpea-buffer-prop-get-list "filetags" "[ :]"))

  (defun vulpea-buffer-tags-set (&rest tags)
    "Set TAGS in current buffer.
  If filetags value is already set, replace it."
    (if tags
        (vulpea-buffer-prop-set
         "filetags" (concat ":" (string-join tags ":") ":"))
      (vulpea-buffer-prop-remove "filetags")))

  (defun vulpea-buffer-tags-add (tag)
    "Add a TAG to filetags in current buffer."
    (let* ((tags (vulpea-buffer-tags-get))
           (tags (append tags (list tag))))
      (apply #'vulpea-buffer-tags-set tags)))

  (defun vulpea-buffer-tags-remove (tag)
    "Remove a TAG from filetags in current buffer."
    (let* ((tags (vulpea-buffer-tags-get))l
           (tags (delete tag tags)))
      (apply #'vulpea-buffer-tags-set tags)))

  (defun vulpea-buffer-prop-set (name value)
    "Set a file property called NAME to VALUE in buffer file.
  If the property is already set, replace its value."
    (setq name (downcase name))
    (org-with-point-at 1
      (let ((case-fold-search t))
        (if (re-search-forward (concat "^#\\+" name ":\\(.*\\)")
                               (point-max) t)
            (replace-match (concat "#+" name ": " value) 'fixedcase)
          (while (and (not (eobp))
                      (looking-at "^[#:]"))
            (if (save-excursion (end-of-line) (eobp))
                (progn
                  (end-of-line)
                  (insert "\n"))
              (forward-line)
              (beginning-of-line)))
          (insert "#+" name ": " value "\n")))))

  (defun vulpea-buffer-prop-set-list (name values &optional separators)
    "Set a file property called NAME to VALUES in current buffer.
  VALUES are quoted and combined into single string using
  `combine-and-quote-strings'.
  If SEPARATORS is non-nil, it should be a regular expression
  matching text that separates, but is not part of, the substrings.
  If nil it defaults to `split-string-default-separators', normally
  \"[ \f\t\n\r\v]+\", and OMIT-NULLS is forced to t.
  If the property is already set, replace its value."
    (vulpea-buffer-prop-set
     name (combine-and-quote-strings values separators)))

  (defun vulpea-buffer-prop-get (name)
    "Get a buffer property called NAME as a string."
    (org-with-point-at 1
      (when (re-search-forward (concat "^#\\+" name ": \\(.*\\)")
                               (point-max) t)
        (buffer-substring-no-properties
         (match-beginning 1)
         (match-end 1)))))

  (defun vulpea-buffer-prop-get-list (name &optional separators)
    "Get a buffer property NAME as a list using SEPARATORS.
  If SEPARATORS is non-nil, it should be a regular expression
  matching text that separates, but is not part of, the substrings.
  If nil it defaults to `split-string-default-separators', normally
  \"[ \f\t\n\r\v]+\", and OMIT-NULLS is forced to t."
    (let ((value (vulpea-buffer-prop-get name)))
      (when (and value (not (string-empty-p value)))
        (split-string-and-unquote value separators))))

  (defun vulpea-buffer-prop-remove (name)
    "Remove a buffer property called NAME."
    (org-with-point-at 1
      (when (re-search-forward (concat "\\(^#\\+" name ":.*\n?\\)")
                               (point-max) t)
        (replace-match ""))))

  )
(require 'vulpea)
(add-hook 'find-file-hook #'vulpea-project-update-tag)
(add-hook 'before-save-hook #'vulpea-project-update-tag)
(advice-add 'org-agenda :before #'vulpea-agenda-files-update)
(advice-add 'org-todo-list :before #'vulpea-agenda-files-update)



;; (use-package emacsql-sqlite-module
;;   :after org-roam)
;; 8. paper

(provide 'init-org)
