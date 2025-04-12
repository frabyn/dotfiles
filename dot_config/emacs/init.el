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
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Use straight.el for use-package expressions
(straight-use-package 'use-package)

;; Use straight.el by default with use-package
(setq straight-use-package-by-default t)

;;;; Performance

;; Set a reasonable GC threshold for ongoing operation
(setq gc-cons-threshold (* 16 1000 1000))

;; Add periodic garbage collection during idle time
(use-package gcmh
  :demand t
  :config
  (gcmh-mode 1))

;;;; Basic UI Configuration

;; Size initial frame
(add-to-list 'default-frame-alist '(height . 30))
(add-to-list 'default-frame-alist '(width . 100))

;; Set default font face
(set-face-attribute 'default nil :font "BerkeleyMono Nerd Font" :height 160)

;; Disable the menu bar
(menu-bar-mode -1)

;; Disable the tool bar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (set-fringe-mode 10))

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
(recentf-mode 1)

;; Simplify yes/no prompts
(defalias 'yes-or-no-p #'y-or-n-p)

;; Sentence formatting
(setq sentence-end-double-space nil) ; you monsters

;; Backups
(setq make-backup-files nil) ; stop creating ~ files

;;;; Helper Interfaces & Completion

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package vertico
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("C-f" . vertico-exit)
              :map minibuffer-local-map
              ("M-h" . backward-kill-word))
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

;; Configure directory extension.
(use-package vertico-directory
  :straight nil
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;;;; Git Integration

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; Bind the `magit-status' command to a convenient key.
(global-set-key (kbd "C-c g") #'magit-status)

;;;; Development Environment

;; Enabled inline static analysis
(add-hook 'prog-mode-hook #'flymake-mode)

;; Environment management
(use-package envrc
  :hook (after-init . envrc-global-mode))

;; Eglot Config 
(add-hook 'prog-mode-hook #'eglot-ensure)
(with-eval-after-load 'eglot
  (setq eglot-autoshutdown t)
  (define-key eglot-mode-map (kbd "C-c l r") #'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c l a") #'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c l f") #'eglot-format)
  (define-key eglot-mode-map (kbd "C-c l d") #'eglot-find-declaration)
  (define-key eglot-mode-map (kbd "C-c l i") #'eglot-find-implementation))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(python-mode . ("pyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(python-mode . ("ruff" "server")))
  (add-to-list 'eglot-server-programs
               '(text-mode . ("harper-ls" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(markdown-mode . ("harper-ls" "--stdio"))))

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

;; Autocompletion

;; Enable Completion Preview mode in various buffers
(add-hook 'prog-mode-hook #'completion-preview-mode)
(add-hook 'text-mode-hook #'completion-preview-mode)
(with-eval-after-load 'comint
  (add-hook 'comint-mode-hook #'completion-preview-mode))

(with-eval-after-load 'completion-preview
  ;; Show the preview already after two symbol characters
  (setq completion-preview-minimum-symbol-length 2)

  ;; Non-standard commands to that should show the preview:
  ;; Org mode has a custom `self-insert-command'
  (push 'org-self-insert-command completion-preview-commands)
  ;; Paredit has a custom `delete-backward-char' command
  (push 'paredit-backward-delete completion-preview-commands)

  ;; Bindings that take effect when the preview is shown:
  ;; Cycle the completion candidate that the preview shows
  (keymap-set completion-preview-active-mode-map "M-n" #'completion-preview-next-candidate)
  (keymap-set completion-preview-active-mode-map "M-p" #'completion-preview-prev-candidate)
  ;; Convenient alternative to C-i after typing one of the above
  (keymap-set completion-preview-active-mode-map "M-i" #'completion-preview-insert))

;; Add extensions
(use-package cape
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind ("C-c p" . cape-prefix-map) 
  :init
  ;; Add to the global default value of `completion-at-point-functions'
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

;;;; File Explorer

;; Treemacs
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-files-by-mouse-dragging    t
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-nerd-icons
  :after treemacs
  :config
  (treemacs-load-theme "nerd-icons"))

;;;; Language-Specific Configuration

;; Python
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-hook 'python-mode-hook #'eglot-ensure)

;; Mail
(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))

;; YAML Support
(use-package yaml-mode)

;; Markdown support
(use-package markdown-mode)

;;;; Miscellaneous

;; Store automatic customisation options elsewhere
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Reduce warnings
(setq warning-minimum-level :error)

(provide 'init)
;;; init.el ends here
