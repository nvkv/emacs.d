(require 'package)

;; Set PATH
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "~/go/bin/")
(add-to-list 'exec-path "~/.cargo/bin/")
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setenv "DICPATH" (concat (getenv "HOME") "/Library/Spelling"))

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
  (menu-bar-mode 1)
  (global-visual-line-mode 1)
  (setq-default tab-width 2)
  (setq sh-basic-offset 2)
  (set-window-buffer nil (current-buffer))
  (setq ring-bell-function 'ignore)
  (setq-default left-margin-width 0 right-margin-width 0)
  (windmove-default-keybindings)
  (set-cursor-color "#000000")

  :custom
  (inhibit-startup-screen t "Don't show splash screen")
  (use-dialog-box nil "Disable dialog boxes")
  (enable-recursive-minibuffers t "Allow minibuffer commands in the minibuffer")
  (indent-tabs-mode nil "Spaces!")
  (create-lockfiles nil)
  (show-paren-mode 1)
  (debug-on-quit nil)
  (cursor-type 'box)

  :bind
  ("M-i" . imenu)
  ("C-S-<down>" . enlarge-window)
  ("C-S-<up>" . shrink-window)
  ("C-S-<left>" . shrink-window-horizontally)
  ("C-S-<right>" . enlarge-window-horizontally))


(use-package ispell
  :defer t
  :custom
  (ispell-local-dictionary-alist
    '(("russian"
       "[АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯабвгдеёжзийклмнопрстуфхцчшщьыъэюя]"
       "[^АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯабвгдеёжзийклмнопрстуфхцчшщьыъэюя]"
       "[-]"  nil ("-d" "ru") nil utf-8)
      ("english"
       "[A-Za-z]" "[^A-Za-z]"
       "[']"  nil ("-d" "en_GB") nil utf-8)))
  (ispell-program-name "aspell")
  (ispell-dictionary "russian")
  (ispell-really-aspell t)
  (ispell-really-hunspell nil)
  (ispell-encoding8-command t)
  (ispell-silently-savep t))


(use-package flyspell
  :ensure t
  :custom
  (flyspell-delay 1)
  :hook
  (text-mode . (lambda () (flyspell-mode 1))))

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
  (auto-save-file-name-transforms
   `((".*" ,temporary-file-directory t)))
  (delete-old-versions t)
  (kept-new-versions 6)
  (kept-old-versions 2)
  (version-control t))

(use-package faces
  :ensure nil
  :if window-system
  :init
  (set-frame-font "Go Mono-17"))

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

(use-package ediff
  :ensure nil
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package highlighter
  :load-path "site-lisp/highlighter/")

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

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package highlight-indent-guides
  :ensure t
  :config
  (setq highlight-indent-guides-method 'character)
  :hook
  (yaml-mode . highlight-indent-guides-mode)
  (python-mode . highlight-indent-guides-mode))

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
  (setq-default gofmt-command "~/go/bin/goimports")
  :hook
  (before-save-hook . gofmt-before-save))

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

(use-package olivetti
  :ensure t
  :bind
  ("C-c o" . olivetti-mode))

(use-package deft
  :ensure t
  :config
  (setq deft-directory "~/notes"
        deft-extensions '("org" "md" "txt")
        deft-default-extension (car deft-extensions)
        deft-recursive t
        deft-use-filename-as-title t
        deft-use-filter-string-for-filename t
        deft-auto-save-interval 30.0
        deft-file-naming-rules '((noslash . "-")
                                 (nospace . "-")
                                 (case-fn . downcase)))
  :bind
  ("C-c d" . deft))

(use-package reveal-in-osx-finder
  :ensure t
  :bind
  ("C-c <SPC>" . reveal-in-osx-finder))

(use-package org-bullets
  :ensure t
  :custom
  (org-bullets-bullet-list '("•"))
  (org-ellipsis "↴")
  :hook
  (org-mode . org-bullets-mode))

(use-package ox-gfm
  :ensure t
  :init
  (eval-after-load "org"
    '(require 'ox-gfm nil t)))

(use-package org-preview-html
  :ensure t)

(use-package eros
  :ensure t
  :config
  (eros-mode 1))

(use-package restclient
  :ensure t)

(use-package reverse-im
  :ensure t
  :config
  (reverse-im-activate "russian-no-windows"))

(use-package rust-mode
  :ensure t
  :config
  (setq rust-format-on-save t))

(use-package cargo
  :ensure t
  :hook
  (rust-mode . cargo-minor-mode)
  (rust-mode . company-mode))

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.5)
  (setq company-show-numbers t)
  (company-tng-configure-default)
  (setq company-frontends
        '(company-tng-frontend
          company-pseudo-tooltip-frontend
          company-echo-metadata-frontend)))

(use-package company-tabnine
  :ensure t
  :config
  (add-to-list 'company-backends #'company-tabnine))

(use-package ripgrep
  :ensure t)
