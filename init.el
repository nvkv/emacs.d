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
(setq-default tab-width 2)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq-default left-margin-width 0 right-margin-width 0)
(set-window-buffer nil (current-buffer))

(when window-system
  (progn
    (tool-bar-mode 0)
    (menu-bar-mode 1)
    (scroll-bar-mode -1)
    (blink-cursor-mode -1)))

(server-start)

(if (fboundp 'mac-auto-operator-composition-mode)
    (mac-auto-operator-composition-mode))

(when (window-system)
	(progn
		(load "~/.emacs.d/lisp/fira-code")
		(set-frame-font "Fira Code Retina-18")))

(load "~/.emacs.d/lisp/russian-nowinkeys")
(setq default-input-method "russian-no-windows")

(use-package solarized-theme
  :if window-system
  :ensure t
  :defer nil
  :config
  (load-theme 'solarized-light t))

(use-package github-modern-theme
	:if window-system
  :ensure t
  :defer t
  :config
  (load-theme 'github-modern t))

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
    (setq ac-delay 1)
    (setq ac-use-menu-map t)
    (setq ac-menu-height 50)
    (setq ac-use-quick-help nil)
    (setq ac-comphist-file  "~/.emacs.d/ac-comphist.dat")
    (setq ac-ignore-case nil)
    (setq ac-dwim t)
    (setq ac-fuzzy-enable t)))

(use-package projectile
  :ensure t
  :config
  (projectile-mode))

(use-package yaml-mode
  :ensure t)

(use-package git-commit
  :if window-system
  :ensure nil)

(use-package terraform-mode
  :ensure t)

(use-package go-mode
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package dockerfile-mode
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
		(soft-stone-theme stekene minimal-theme monochrome monochrome-theme farmhouse-theme basic-theme eziam-common eziam-theme github-modern-theme dockerfile-mode markdown-mode go-mode eink-theme github-theme yaml-mode use-package terraform-mode solarized-theme projectile git-commit editorconfig auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
