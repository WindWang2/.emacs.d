;; config the org-roam-bibtex
(use-package org-roam-bibtex
  :defer t
  :after org-roam
  :diminish org-roam-bibtex-mode
  :init
  (org-roam-bibtex-mode 1)
  :config
  (setq orb-roam-ref-format 'org-cite))


(use-package bibtex
  :config
  (setq bibtex-file-path (concat own-org-directory "references/bibs/")
        bibtex-files '("NSF_Fund.bib" "buildings-self-sup.bib" "landfills.bib")
        bibtex-notes-path (concat own-org-directory "paper_notes")
        bibtex-align-at-equal-sign t
        bibtex-autokey-titleword-separator "-"
        bibtex-autokey-year-title-separator "-"
        bibtex-autokey-name-year-separator "-"
        bibtex-dialect 'biblatex))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act))
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package citar-embark
  :after citar embark
  :no-require
  :diminish citar-embark-mode
  :config (citar-embark-mode))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

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
        citar-citeproc-csl-styles-dir (concat own-org-directory "references/csl_files")
        citar-citeproc-csl-locales-dir (concat own-org-directory "references/locals")
        bibtex-file-path (concat own-org-directory "references/bibs/")
        citar-format-reference-function #'citar-citeproc-format-reference
        citar-file-open-prompt nil)
  (setq citar-templates
        '((main . "${author editor:30}     ${date year issued:4}     ${title:48}")
          (suffix . "          ${=key= id:15}    ${=type=:12}    ${tags keywords:*}")
          (preview . "${author editor} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
          (note . "Scholar: ${title}")))
  :config
  (setq citar-at-point-function 'embark-act
        citar-bibliography (mapcar (lambda (file) (concat bibtex-file-path file)) bibtex-files)
        citar-library-paths `(,(concat bibtex-file-path "../PDFs/"))
        citar-notes-paths `(,bibtex-notes-path))

  (defun citar-copy-title (citekey)
    "Copy title associated with the CITEKEYS."
    (interactive (list (citar-select-ref)))
    (setq ref-title (citar-get-value 'title citekey))
    (if (not (equal "" ref-title))
        (progn
          (kill-new (concat ref-title (format "  [cite:@%s]" citekey)))
          (message (format "Copied:\n%s" ref-title)))
      (message "Key not found.")))

  (use-package citar-org-roam
    :after citar org-roam
    :no-require
    :config
    (citar-org-roam-mode)
    (setq citar-org-roam-note-title-template "Scholar: ${title}\n#+filetags: paper_note")
    (setq citar-org-roam-subdir "paper_notes")
    (defun citar-org-roam--ebib-create-note (citekey entry)
      (ebib-popup-note citekey))
    (setq citar-org-roam-notes-config (list :name "Org-Roam Notes ebib"
                                            :category 'org-roam-node
                                            :items #'citar-org-roam--get-candidates
                                            :hasitems #'citar-org-roam-has-notes
                                            :open #'citar-org-roam-open-note
                                            :create #'citar-org-roam--ebib-create-note))
    (citar-org-roam-setup))
  )
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
  (ebib-index-window-size 15)
  (ebib-reading-list-file (concat own-org-directory "paper_notes/reading_list.org"))
  (ebib-keywords-field-keep-sorted t)
  (ebib-keywords-save-on-exit 'always)
  (ebib-use-timestamp t)
  (ebib-filters-default-file (concat bibtex-file-path "ebib-filters"))
  (ebib-file-associations '(("pdf")))
  (ebib-index-columns '(("Entry Key" 20 t)
			            ("Author/Editor" 15 nil)
			            ("Year" 6 t)
			            ("Title" 50 t)))
  (ebib-notes-storage 'one-file-per-note)
  (ebib-index-default-sort '("timestamp" . descend))
  :config
  ;; (w32-shell-execute "open" file)
  (when (eq system-type 'windows-nt)
    (modify-coding-system-alist 'process "c:/Users/Administrator/AppData/Local/SumatraPDF/SumatraPDF.exe" '(utf-8 . chinese-gbk-dos))
    (setq ebib-file-associations '(("pdf" . "c:/Users/Administrator/AppData/Local/SumatraPDF/SumatraPDF.exe")))
    ;; (setq ebib-file-associations '(("pdf" . "")))
    )
  (when sys/linuxp
    (setq ebib-file-associations '(("pdf" . "evince")))
    )
  (when sys/wsl
    (setq ebib-file-associations '(("pdf" . "wslview"))))
  (when sys/macp
    (setq ebib-file-associations '(("pdf" . "open")))
    )
  (setq ebib-notes-template ":PROPERTIES:\n%K\n:ROAM_REFS: @%k\n:ID:  %i\n:NOTER_DOCUMENT: %F\n:END:\n#+filetags: paper_note\n%%?#+TITLE: Scholar: %X\n \n[cite:@%k]\nDate: %S\n* Main Idea \n\n* Comments \n\n* Details \n\n* Highlights\n%%?"
        ebib-reading-list-template-specifiers '((?K . ebib-reading-list-create-org-identifier)
                                                (?T . ebib-create-org-title)
                                                (?M . ebib-reading-list-todo-marker)
                                                (?L . ebib-create-org-link)
                                                (?F . ebib-create-org-file-link)
                                                (?D . ebib-create-org-doi-link)
                                                (?U . ebib-create-org-url-link)
                                                (?k . ebib-create-key)
                                                (?i . ebib-create-id))
        ebib-reading-list-template "* %M %T\n:PROPERTIES:\n%K\n:ID: %i\n:END:\n[cite:@%k]\n"
        ebib-notes-directory (concat own-org-directory "paper_notes")
        ebib-notes-locations `(,(concat own-org-directory "paper_notes"))
        ;; ebib-notes-default-file (concat bibtex-file-path "../paper_notes/notes.org")
        ebib-keywords (concat bibtex-file-path "keywords.txt")
        ebib-preload-bib-files `(,(concat bibtex-file-path "NSF_Fund.bib"))
        ebib-file-search-dirs (list (concat bibtex-file-path "../PDFs")
                                    (concat bibtex-file-path "../bibs"))
        ebib-notes-template-specifiers '((?k . ebib-create-key)
                                         (?i . ebib-create-id)
                                         (?K . ebib-create-org-identifier)
                                         (?X . ebib-create-org-title)
    		                             (?T . ebib-create-org-description)
                                         (?L . ebib-create-org-link)
                                         (?F . ebib-create-org-file-name)
                                         (?S . ebib-create-org-time-stamp)))
  ;; Rename of ~ebib-import-file~, Ref: https://mtino1594.hatenablog.com/entry/2019/02/04/230032
  (defun my/get-file-name (name)
    (if (> (length name) 50) (substring name 0 50) name))
  (defun my/ebib-name-transform-function (key)
    "Serach file"
    (format "%s_%s_%s"
            (ebib-get-year-for-display key ebib--cur-db)
            (substring key (string-match "[A-Za-z]+" key) (match-end 0))
            (my/get-file-name (replace-regexp-in-string "?" "_" (replace-regexp-in-string ":" "_" (replace-regexp-in-string " " "_" (ebib-create-org-title key ebib--cur-db)))))))

  (setq ebib-name-transform-function #'my/ebib-name-transform-function)

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

;; (setq org-latex-pdf-process
;;       '("xelatex -interaction nonstopmode -disable-write18 -shell-escape -output-directory %o %f"
;;         "biber --output-directory %o $(basename %f .tex)"
;;         "xelatex -interaction nonstopmode -disable-write18 -shell-escape -output-directory %o %f"
;;         "xelatex -interaction nonstopmode -disable-write18 -shell-escape -output-directory %o %f")
;;       )
;; ;;
;; ref: https://www.skfwe.cn/p/org-%E9%80%9A%E8%BF%87latex%E5%AF%BC%E5%87%BA-pdf/
(setq org-latex-pdf-process
      '("xelatex -shell-escape -interaction=nonstopmode -output-directory %o %f"
        "bibtex %b"
        "xelatex -shell-escape -interaction=nonstopmode -output-directory %o %f"
        "xelatex -shell-escape -interaction=nonstopmode -output-directory %o %f"))
;; (setq  org-latex-pdf-process '("tectonic -Z shell-escape %f"))
;; (setq org-latex-pdf-process
;;       '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;         "bibtex %b"
;;         "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;         "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(add-to-list 'org-latex-classes
             '("mdpi"
               "\\documentclass[remotesensing,article,submit,pdftex,moreauthors]{Templates/MDPI/Definitions/mdpi}\n\\newlength{\\extralength}\n\\setlength{\\extralength}{1.5cm}\n\\setlength{\\parskip}{0.3em}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
             )
(add-to-list 'org-latex-classes
             '("koma-article" "\\documentclass[11pt]{scrartcl}\n\\newlength{\\extralength}\n\\setlength{\\extralength}{1.5cm}\n\\setlength{\\parskip}{0.3em}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
(add-to-list 'org-latex-classes
             '("koma-letter" "\\documentclass[11pt]{scrletter}\n\\newlength{\\extralength}\n\\setlength{\\extralength}{1.5cm}\n\\setlength{\\parskip}{0.3em}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
(add-to-list 'org-latex-classes
             '("koma-report" "\\documentclass[11pt]{scrreprt}\n\\newlength{\\extralength}\n\\setlength{\\extralength}{1.5cm}\n\\setlength{\\parskip}{0.3em}"
               ("\\part{%s}" . "\\part*{%s}")
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
(add-to-list 'org-latex-classes
             '("koma-book" "\\documentclass[11pt]{scrbook}\n\\newlength{\\extralength}\n\\setlength{\\extralength}{1.5cm}\n\\setlength{\\parskip}{0.3em}"
               ("\\part{%s}" . "\\part*{%s}")
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
(add-to-list 'org-latex-classes
             '("ctex-article" "\\documentclass[11pt]{ctexart}\n\\newlength{\\extralength}\n\\setlength{\\extralength}{1.5cm}\n\\setlength{\\parskip}{0.3em}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
(add-to-list 'org-latex-classes
             '("ctex-book" "\\documentclass[11pt]{ctexbook}\n\\newlength{\\extralength}\n\\setlength{\\extralength}{1.5cm}\n\\setlength{\\parskip}{0.3em}"
               ("\\part{%s}" . "\\part*{%s}")
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
(add-to-list 'org-latex-classes
             '("ctex-report" "\\documentclass[11pt]{ctexrep}\n\\newlength{\\extralength}\n\\setlength{\\extralength}{1.5cm}\n\\setlength{\\parskip}{0.3em}"
               ("\\part{%s}" . "\\part*{%s}")
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))


;; (setq org-latex-with-hyperref nil)
;; You can disable in the org file with the sample as follows:
;; # Local Variables:
;; # org-latex-default-packages-alist: nil
;; # org-latex-with-hyperref: nil
;; # org-latex-packages-alist: nil
;; # End:
;; remove the hyperref
(setq org-latex-packages-alist
      '(("" "capt-of" nil)
        ("" "tabularx" nil)
        ("" "float" nil)
        ("" "booktabs" nil)
        ("" "changepage" nil)
        ("" "multirow" nil)
        ("" "caption" nil)
        ("" "newfloat" nil)
        ("" "marginfix" nil)
        ("" "geometry" nil)
        ("" "lipsum" nil)
        ("" "seqsplit" nil)
        ("" "tabu" nil)
        ("" "setspace" nil)
        ("" "color" nil)
        ))


(setq org-format-latex-header "\\documentclass{article}\n\\usepackage[usenames]{color}\n[DEFAULT-PACKAGES]\n[PACKAGES]\n\\pagestyle{empty}             % do not remove\n% The settings below are copied from fullpage.sty\n\\setlength{\\textwidth}{\\paperwidth}\n\\addtolength{\\textwidth}{-3cm}\n\\setlength{\\oddsidemargin}{1.5cm}\n\\addtolength{\\oddsidemargin}{-2.54cm}\n\\setlength{\\evensidemargin}{\\oddsidemargin}\n\\setlength{\\textheight}{\\paperheight}\n\\addtolength{\\textheight}{-\\headheight}\n\\addtolength{\\textheight}{-\\headsep}\n\\addtolength{\\textheight}{-\\footskip}\n\\addtolength{\\textheight}{-3cm}\n\\setlength{\\topmargin}{1.5cm}\n\\addtolength{\\topmargin}{-2.54cm}")

;; Export to latex without "\title{}, should include the empty title option in the org file"
(defun my-org-latex-remove-title (str)
  (replace-regexp-in-string "^\\\\title{}$" "" str))
(defun my-org-latex-remove-date (str)
  (replace-regexp-in-string "^\\\\date{}$" "" str))

(advice-add 'org-latex-template :filter-return 'my-org-latex-remove-title)
(advice-add 'org-latex-template :filter-return 'my-org-latex-remove-date)

;; https://emacs.stackexchange.com/questions/47733/org-latex-exports-math-as-can-this-be-avoided
;; $$ -> $$
(defun org-latex-math-block (_math-block contents _info)
  "Transcode a MATH-BLOCK object from Org to LaTeX.
CONTENTS is a string.  INFO is a plist used as a communication
channel."
  (when (org-string-nw-p contents)
    (format "$%s$" (org-trim contents))))

;; (setq org-latex-listings 'minted)
;; (defvar minted-cache-dir
;;   (file-name-as-directory
;;    (cond (sys/macp    (expand-file-name ".minted/jombname"
;;                                         temporary-file-directory))
;;          (sys/win32p    (expand-file-name ".minted/\\jombname"
;;                                           temporary-file-directory))
;;          (sys/linuxp    (expand-file-name ".minted/jombname"
;;                                           temporary-file-directory))
;;          )))
;; (add-to-list 'org-latex-packages-alist
;;              `(,(concat "cachedir=" minted-cache-dir)
;;                "minted" nil))
;; (setq org-latex-packages-alist '(("" "minted")))
;; (setq org-latex-minted-options '(("breaklines" "true")
;;                                  ("breakanywhere" "true")))

;; for latex preview process  ---------------------------------------------------------- <2022-10-29 周六>

;; ref: https://emacs-china.org/t/org-latex-preview/22288/8

(when sys/win32p
  (setq org-preview-latex-process-alist
        '((dvisvgm
           :programs ("latex" "dvisvgm")
           :description "dvi > svg"
           :message "you need to install the programs: latex and dvisvgm."
           :image-input-type "dvi"
           :image-output-type "svg"
           :image-size-adjust (1.0 . 1.0)
           :latex-compiler ("latex -interaction nonstopmode -shell-escape -output-directory %o %f")
           :image-converter ("dvisvgm %f -e -n -b 1 -c %S -o %O")))))
(when sys/macp
  (setq org-preview-latex-process-alist
        '((dvisvgm
           :programs ("latex" "dvisvgm")
           :description "dvi > svg"
           :message "you need to install the programs: latex and dvisvgm."
           :image-input-type "dvi"
           :image-output-type "svg"
           :image-size-adjust (1.7 . 1.5)
           :latex-compiler ("latex -interaction nonstopmode -shell-escape -output-directory %o %f")
           :image-converter ("dvisvgm %f -e -n -b 1 -c %S -o %O")))))

;; (add-hook 'org-mode-hook (lambda () (org-fragtog-mode -1)))
(setq org-preview-latex-default-process 'dvisvgm)
(defun my/org--latex-header-preview (orig &rest args)
  "Setup dedicated `org-format-latex-header' to `my/org--match-text-baseline-ascent'."
  (let ((org-format-latex-header
         "\\documentclass[preview]{standalone}
\\usepackage{dsfont}
\\usepackage[usenames]{color}
[PACKAGES]
[DEFAULT-PACKAGES]"))
    (apply orig args)))

(defun my/org--match-text-baseline-ascent (imagefile)
  "Set `:ascent' to match the text baseline of an image to the surrounding text.
Compute `ascent' with the data collected in IMAGEFILE."
  (advice-add 'org-create-formula-image :around #'my/org--latex-header-preview)
  (let* ((viewbox (split-string
                   (xml-get-attribute (car (xml-parse-file imagefile)) 'viewBox)))
         (min-y (string-to-number (nth 1 viewbox)))
         (height (string-to-number (nth 3 viewbox)))
         (ascent (round (* -100 (/ min-y height)))))
    (if (or (< ascent 0) (> ascent 100))
        'center
      ascent)))

(defun org--make-preview-overlay (beg end image &optional imagetype)
  "Build an overlay between BEG and END using IMAGE file.
Argument IMAGETYPE is the extension of the displayed image,
as a string.  It defaults to \"png\"."
  (let ((ov (make-overlay beg end))
	    (imagetype (or (intern imagetype) 'png)))
    (let ((ascent (my/org--match-text-baseline-ascent image)))
      (overlay-put ov 'org-overlay-type 'org-latex-overlay)
      (overlay-put ov 'evaporate t)
      (overlay-put ov
		           'modification-hooksn
		           (list (lambda (o _flag _beg _end &optional _l)
			               (delete-overlay o))))
      (overlay-put ov
		           'display
		           (list 'image :type imagetype :file image :ascent ascent)))))

(provide 'init-latex)
