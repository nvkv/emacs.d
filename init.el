(require 'package)

(setq package-archives
      `(,@package-archives
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(setq package-enable-at-startup nil)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq ring-bell-function 'ignore)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(menu-bar-mode 0)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(server-start)
;(show-paren-mode 1)

(setq-default left-margin-width 2 right-margin-width 2)
(set-window-buffer nil (current-buffer))

(load "~/.emacs.d/lisp/fira-code")
(load "~/.emacs.d/lisp/russian-nowinkeys")
(setq default-input-method "russian-no-windows")

(use-package solarized-theme
  :ensure t
  :defer t
  :config
  (load-theme 'solarized-light t))

(use-package github-theme
  :ensure t
  :defer nil
  :config
  (load-theme 'github t))

(use-package eink-theme
  :ensure t
  :defer t
  :config
  (load-theme 'eink t))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package auto-complete
  :ensure t
  :init
  (progn
    (auto-complete-mode t))
  :config
  (progn 
    (use-package auto-complete-config)
    (ac-set-trigger-key "TAB")
    (ac-config-default)
    (setq ac-delay 0.02)
    (setq ac-use-menu-map t)
    (setq ac-menu-height 50)
    (setq ac-use-quick-help nil) 
    (setq ac-comphist-file  "~/.emacs.d/ac-comphist.dat")
    (setq ac-ignore-case nil)
    (setq ac-dwim  t)
    (setq ac-fuzzy-enable t)))

(use-package projectile
  :ensure t
  :config
  (projectile-mode))

(use-package yaml-mode
  :ensure t)

(use-package git-commit
  :ensure t)

(use-package terraform-mode
  :ensure t)

(defun remap-faces-default-attributes ()
  (let ((family (face-attribute 'default :family))
	(height (face-attribute 'default :height)))
    (mapcar (lambda (face)
              (face-remap-add-relative
               face :family family :weight 'normal :height height))
	    (face-list))))

(when (display-graphic-p)
  (add-hook 'minibuffer-setup-hook 'remap-faces-default-attributes)
  (add-hook 'change-major-mode-after-body-hook 'remap-faces-default-attributes))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(package-selected-packages
   (quote
    (eink-theme github-theme yaml-mode use-package terraform-mode solarized-theme projectile git-commit editorconfig auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
