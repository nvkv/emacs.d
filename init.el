(require 'package)
(setenv "LC_ALL" "en_US.UTF-8")

(defun on-windows? ()
  (eq system-type 'windows-nt))

;; Package bootstrap
(setq package-archives
      `(,@package-archives
        ("melpa" . "https://melpa.org/packages/")))

(if (on-windows?)
    (setenv "PATH" (concat "C:\\msys64\\usr\\bin;" (getenv "PATH")))
    (setq exec-path (append exec-path '("C:/msys64/usr/bin"))))

(package-initialize)
(setq package-enable-at-startup nil)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(server-start)

(use-package gnu-elpa-keyring-update
  :ensure t
  :if (on-windows?))

(use-package exec-path-from-shell
  :ensure t
  :if (not (on-windows?))
  :config
  (exec-path-from-shell-initialize))

(use-package emacs
  :ensure nil
  :init
  (put 'narrow-to-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (scroll-bar-mode 0)
  (blink-cursor-mode 0)
  (tool-bar-mode 0)
  (menu-bar-mode 0)
  (global-visual-line-mode 1)
  (setq-default tab-width 2)
  (setq sh-basic-offset 2)
  (set-window-buffer nil (current-buffer))
  (setq ring-bell-function 'ignore)
  (setq-default left-margin-width 0 right-margin-width 0)
  (windmove-default-keybindings)
  (setq w32-get-true-file-attributes nil)
  (setq split-height-threshold 80)
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
  :if (not (on-windows?))
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
  :if (not (on-windows?))
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
  (make-backup-files nil)
  (delete-old-versions t)
  (kept-new-versions 6)
  (kept-old-versions 2)
  (version-control t))

(use-package faces
  :ensure nil
  :if window-system
  :init
  (set-frame-font "Iosevka Curly-14"))

(use-package mule
  :ensure nil
  :if (not (on-windows?))
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
  :ensure t
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

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package projectile
  :ensure t
  :custom
  (projectile-indexing-method 'alien)
  :config
  (setq-default projectile-enable-caching t)
  (projectile-mode 1)
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
  (if (on-windows?)
      (progn
       (setq magit-git-executable "C:/Program Files/Git/cmd/git.exe")
       (setq magit-refresh-verbose nil)))
  (global-set-key (kbd "C-x g") 'magit-status))

(use-package terraform-mode
  :ensure t
  :hook (terraform-mode . terraform-format-on-save-mode))

(use-package go-mode
  :ensure t
  :init
  (if (on-windows?)
      (setq-default gofmt-command "C:/Users/nvkv/go/bin/goimports.exe")
      (setq-default gofmt-command "~/go/bin/goimports"))
  :hook
  (before-save . gofmt-before-save))

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

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package olivetti
  :ensure t
  :bind
  ("C-c o" . olivetti-mode))

(use-package reveal-in-osx-finder
  :ensure t
  :if (not (on-windows?))
  :bind
  ("C-c <SPC>" . reveal-in-osx-finder))

(use-package eros
  :ensure t
  :config
  (eros-mode 1))

(use-package restclient
  :ensure t)

(use-package reverse-im
  :ensure t
  :config
  (reverse-im-activate "russian-computer"))

(use-package rust-mode
  :ensure t
  :config
  (setq rust-format-on-save t))

(use-package cargo
  :ensure t
  :hook
  (rust-mode . cargo-minor-mode))

(use-package ripgrep
  :ensure t)

(use-package powershell
  :ensure t)

(use-package mood-line
  :ensure t
  :hook
  (after-init . mood-line-mode))
