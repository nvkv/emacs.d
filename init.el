(require 'package)

;; Set PATH
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "~/go/bin/")
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))

;; Package bootstrap
(setq package-archives
      `(,@package-archives
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)
(setq package-enable-at-startup nil)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(server-start)

(use-package emacs
  :ensure nil
  :init
  (put 'narrow-to-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (scroll-bar-mode 0)
  (blink-cursor-mode 0)
  (tool-bar-mode 0)
  (menu-bar-mode 0)
  (setq-default tab-width 2)
  (set-window-buffer nil (current-buffer))
  (setq ring-bell-function 'ignore)
  (setq-default left-margin-width 0 right-margin-width 0)

  :custom
  (inhibit-startup-screen t "Don't show splash screen")
  (use-dialog-box nil "Disable dialog boxes")
  (enable-recursive-minibuffers t "Allow minibuffer commands in the minibuffer")
  (indent-tabs-mode nil "Spaces!")
  (create-lockfiles nil)
  (show-paren-mode 1)
  (debug-on-quit nil)

  :bind
  ("M-i" . imenu)
  ("C-S-<down>" . enlarge-window)
  ("C-S-<up>" . shrink-window)
  ("C-S-<left>" . shrink-window-horizontally)
  ("C-S-<right>" . enlarge-window-horizontally))

(use-package files
  :ensure nil
  :hook
  (before-save . delete-trailing-whitespace)
  :custom
  (require-final-newline t)
  ;; backup settings
  (backup-by-copying t)
  (backup-directory-alist
   `((".*" . ,(expand-file-name
               (concat user-emacs-directory "backups")))))
  (delete-old-versions t)
  (kept-new-versions 6)
  (kept-old-versions 2)
  (version-control t))

(use-package faces
  :ensure nil
  :if window-system
  :init
  (set-frame-font "Fira Code-19"))

(use-package mule
  :ensure nil
  :config
  (prefer-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-language-environment "UTF-8")
  (load "~/.emacs.d/site-lisp/russian-no-windows/russian-no-windows.el")
  (setq default-input-method "russian-no-windows"))

(use-package cus-edit
  :ensure nil
  :custom
  (custom-file null-device "Don't store customizations"))

(use-package autorevert
  :ensure nil
  :diminish auto-revert-mode)

(use-package ido
  :ensure nil
  :config
  (ido-mode 1)
  (setq ido-everywhere t)

  (setq ido-enable-flex-matching t))

(use-package the-colour-theme
  :load-path "site-lisp/the-colour-theme/")

(use-package multi-term
  :ensure t

  :config
  (require 'cl)
  (setq multi-term-program "/usr/local/bin/bash")
  (setq multi-term-buffer-name "t")

  (define-key
    term-raw-map
    (kbd "M-<right>")
    '(lambda ()
       (interactive)
       (term-send-raw-string "\e[1;5C")))

  (define-key
    term-raw-map
    (kbd "M-<left>")
    '(lambda ()
       (interactive)
       (term-send-raw-string "\e[1;5D")))

  (define-key
    term-raw-map
    (kbd "M-<backspace>")
    '(lambda ()
       (interactive)
       (term-send-raw-string "\e\d")))

  :bind
  ("C-c t" . multi-term)
  ("C-c C-j" . term-line-mode)
  ("C-c C-k" . term-char-mode)
  ("C-c ]" . multi-term-next)
  ("C-c [" . multi-term-prev))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package ag
  :ensure t)

(use-package auto-complete
  :ensure t
  :init (auto-complete-mode t)
  :config
  (progn
    (use-package auto-complete-config)
    (ac-set-trigger-key "TAB")
    (ac-config-default)
    (setq ac-delay 1)
    (setq ac-use-menu-map t)
    (setq ac-menu-height 50)
    (setq ac-use-quick-help nil)
    (setq ac-comphist-file  "~/.emacs.d/ac-comphist.dat")
    (setq ac-ignore-case nil)
    (setq ac-dwim t)
    (setq ac-fuzzy-enable t)
    (add-to-list 'ac-modes 'terraform-mode)))

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package yaml-mode
  :ensure t)

(use-package git-commit
  :if window-system
  :ensure nil)

(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-x g") 'magit-status))

(use-package terraform-mode
  :ensure t
  :hook (terraform-mode . terraform-format-on-save-mode))

(use-package go-mode
  :ensure t
  :init
  (add-hook 'before-save-hook 'gofmt-before-save)
  (setq-default gofmt-command "~/go/bin/goimports"))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package web-mode
  :ensure t)

(use-package tide
  :ensure t
  :config
  (require 'web-mode)
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (setup-tide-mode)))))

(use-package dockerfile-mode
  :ensure t)

(use-package cider
  :ensure t)

(use-package groovy-mode
  :ensure t)

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package olivetti
  :ensure t
  :bind
  ("C-c o" . olivetti-mode))

(use-package deft
  :ensure t
  :config
  (setq deft-directory "~/Dropbox/notes"
        deft-extensions '("org" "md" "txt")
        deft-default-extension (car deft-extensions)
        deft-recursive t
        deft-use-filename-as-title t
        deft-use-filter-string-for-filename t
        deft-file-naming-rules '((noslash . "-")
                                 (nospace . "-")
                                 (case-fn . downcase)))
  :bind
  ("C-c d" . deft))
