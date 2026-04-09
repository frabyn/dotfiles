;;; package --- Main init file -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; This is my init file.
;;; There are many like it but this one is mine.
;;; My init file is my best friend. It is my life.
;;; I must master it as I must master my life.

;;; Code:

;;;; Package Management

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Use straight.el by default with use-package
(setq straight-use-package-by-default t)

;;;; Performance

;; Let gcmh manage GC from here on
(use-package gcmh
  :demand t
  :config
  (gcmh-mode 1))

;;;; Basic UI Configuration

;; Set default font face
(set-face-attribute 'default nil :font "BerkeleyMono Nerd Font Mono" :height 160)

;; Early-init handles menu/tool/scroll bars via default-frame-alist,
;; but these cover frames created after init and the current frame.
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (set-fringe-mode 10))

;; Undecorated frames
(push '(undecorated . t) default-frame-alist)

;; Disable splash screen
(setq inhibit-startup-screen t)

;; Visual bell to stop beeps
(setq visible-bell t)

;; Window and frame handling
(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)

;;;; Theme & Visual Enhancements

(use-package monokai-theme)
(load-theme 'monokai t)

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

(use-package dashboard
  :config
  (setq dashboard-banner-logo-title nil
        dashboard-startup-banner 'official
        dashboard-center-content t
        dashboard-vertically-center-content t
        dashboard-display-icons-p t
        dashboard-icon-type 'nerd-icons
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-items '((recents . 5)
                          (projects . 5)
                          (agenda . 5))
        dashboard-projects-backend 'project-el
        dashboard-week-agenda t
        dashboard-filter-agenda-entry 'dashboard-no-filter-agenda
        dashboard-force-refresh t)
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name))))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Enable line numbering in `prog-mode'
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;;;; General Editing Configuration

;; Automatically pair parentheses
(electric-pair-mode 1)

;; Display messages when idle, without prompting
(setq help-at-pt-display-when-idle t)

;; Miscellaneous options
(setq-default major-mode
              (lambda () ; guess major mode from file name
                (unless buffer-file-name
                  (let ((buffer-file-name (buffer-name)))
                    (set-auto-mode)))))

;; Enable history and recent files
(save-place-mode 1)
(savehist-mode 1)
(run-with-idle-timer 1 nil #'recentf-mode)

;; Simplify yes/no prompts
(defalias 'yes-or-no-p #'y-or-n-p)

;; Sentence formatting
(setq sentence-end-double-space nil) ; you monsters

;; Backups
(setq make-backup-files nil) ; stop creating ~ files

;;;; Helper Interfaces & Completion

(which-key-mode 1)
(setq which-key-idle-delay 0.3)

(use-package vertico
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package consult
  :bind (("C-x b" . consult-buffer)
         ("C-s" . consult-line)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-s r" . consult-ripgrep)
         ("M-s f" . consult-find)))

;; Configure directory extension.
(use-package vertico-directory
  :straight nil
  :after vertico
  :ensure nil
  :bind (:map vertico-map
            ("RET" . vertico-directory-enter)
            ("DEL" . vertico-directory-delete-char)
            ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; Autocompletion
(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode))

(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package cape
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file))

;;;; Project Management

(setq project-switch-commands '((consult-find "Find file" "f")
                                (consult-ripgrep "Ripgrep" "g")
                                (consult-buffer "Buffer" "b")
                                (magit-project-status "Magit" "m")
                                (project-dired "Dired" "d")
                                (project-eshell "Eshell" "e")))

;;;; Git Integration

(use-package magit
  :defer t
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;;;; Development Environment

;; Enabled inline static analysis
(add-hook 'prog-mode-hook #'flymake-mode)

;; Environment management
(use-package envrc
  :hook (after-init . envrc-global-mode))

;; Eglot Config
(with-eval-after-load 'eglot
  (setq eglot-autoshutdown t)
  (add-to-list 'eglot-server-programs
               '(markdown-mode . ("harper-ls" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(mail-mode . ("harper-ls" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(message-mode . ("harper-ls" "--stdio")))
  (add-to-list 'eglot-server-programs
               `(python-mode . ,(if (executable-find "pylsp")
                                    '("pylsp")
                                  '("uvx" "--with" "pylsp-ruff" "--from" "python-lsp-server" "pylsp"))))
  (keymap-set eglot-mode-map "C-c e r" #'eglot-rename)
  (keymap-set eglot-mode-map "C-c e a" #'eglot-code-actions))

;; Flymake navigation
(with-eval-after-load 'flymake
  (keymap-set flymake-mode-map "M-g n" #'flymake-goto-next-error)
  (keymap-set flymake-mode-map "M-g p" #'flymake-goto-prev-error))

(setq-default eglot-workspace-configuration
              '(:harper-ls (:userDictPath ""
                            :fileDictPath ""
                            :linters (:SpellCheck t
                                      :SpelledNumbers :json-false
                                      :AnA t
                                      :SentenceCapitalization t
                                      :UnclosedQuotes t
                                      :WrongQuotes :json-false
                                      :LongSentences t
                                      :RepeatedWords t
                                      :Spaces t
                                      :Matcher t
                                      :CorrectNumberSuffix t)
                            :codeActions (:ForceStable :json-false)
                            :markdown (:IgnoreLinkTitle :json-false)
                            :diagnosticSeverity "hint"
                            :isolateEnglish :json-false)))

;;;; Terminal

(use-package eat
  :bind (("C-c t" . eat)
         ("C-c T" . eat-project))
  :hook (eshell-load . eat-eshell-mode))

;;;; Dired

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

;;;; Org Mode Configuration

(use-package org
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link))
  :config
  ;; Visual
  (setq org-startup-indented t
        org-hide-emphasis-markers t
        org-pretty-entities t
        org-startup-folded 'content
        org-ellipsis " ▾"
        org-hide-leading-stars t)

  ;; TODO workflow
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w@)" "|" "DONE(d)" "CANCELLED(c@)"))
        org-log-done 'time
        org-log-into-drawer t)

  ;; Agenda
  (setq org-agenda-files '("~/org")
        org-agenda-window-setup 'current-window
        org-agenda-span 10
        org-agenda-start-on-weekday nil)

  ;; Refile
  (setq org-refile-targets '((org-agenda-files :maxlevel . 3))
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil)

  ;; Capture templates
  (setq org-capture-templates
        '(("t" "Task" entry (file "~/org/inbox.org")
           "* TODO %?\n%U\n%a" :empty-lines 1)
          ("n" "Note" entry (file "~/org/inbox.org")
           "* %?\n%U\n%a" :empty-lines 1)
          ("j" "Journal" entry (file+olp+datetree "~/org/journal/journal.org")
           "* %?\n%U" :empty-lines 1)
          ("c" "Clock entry" entry (file "~/org/inbox.org")
           "* TODO %?\n%U" :clock-in t :clock-resume t :empty-lines 1)))

  ;; Clock / timekeeping
  (setq org-clock-persist 'history
        org-clock-in-resume t
        org-clock-out-remove-zero-time-clocks t
        org-clock-report-include-clocking-task t)
  (org-clock-persistence-insinuate)

  ;; Source blocks
  (setq org-confirm-babel-evaluate nil
        org-src-preserve-indentation t
        org-src-tab-acts-natively t)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (emacs-lisp . t)
     (shell . t)))

  :hook
  (org-mode . visual-line-mode))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "��" "●" "○" "●")))

;;;; Language-Specific Configuration

;; Python
(add-hook 'python-mode-hook #'eglot-ensure)
(add-hook 'python-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'eglot-format-buffer nil t)))

;; YAML
(use-package yaml-mode
  :hook (yaml-mode . eglot-ensure))

;; Markdown
(use-package markdown-mode
  :hook ((markdown-mode . visual-line-mode)
         (markdown-mode . eglot-ensure)))

;; Mail (mutt)
(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))
(add-hook 'mail-mode-hook #'visual-line-mode)
(add-hook 'mail-mode-hook #'eglot-ensure)

;; Mail (notmuch)
(use-package notmuch
  :defer t
  :custom
  (notmuch-show-logo nil)
  (notmuch-search-oldest-first nil)
  (notmuch-fcc-dirs nil)
  :hook
  (notmuch-message-mode . visual-line-mode)
  (notmuch-message-mode . eglot-ensure))

;;;; Miscellaneous

;; Store automatic customisation options elsewhere
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Reduce warnings
(setq warning-minimum-level :error)

(provide 'init)
;;; init.el ends here
