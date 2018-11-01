(require 'package)

(add-to-list 'exec-path "/usr/local/bin")
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))

(setq package-archives
      `(,@package-archives
        ("melpa" . "https://melpa.org/packages/")))

;; Org settings
(setq org-directory "~/Dropbox/org/")
(setq org-notes-file (concat org-directory "notes.org"))
(setq org-todo-file (concat org-directory "todo.org"))
(setq org-agenda-files (file-expand-wildcards (concat org-directory "*.org")))
(setq org-default-notes-file (concat org-directory "inbox.org"))
(setq org-refile-targets '((nil :maxlevel . 9)
                           (org-agenda-files :maxlevel . 9)))

(add-hook 'auto-save-hook 'org-save-all-org-buffers)

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline org-default-notes-file "Inbox")
         "* TODO %?\n  %U")
        ("j" "Journal" entry (file+olp+datetree "~/org/journal.org")
         "* %?\nEntered on %U\n  %i")))

(define-key global-map "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-ci" '(lambda ()
                           (interactive)
                           (find-file org-default-notes-file)))

(global-set-key "\C-ct" '(lambda ()
                           (interactive)
                           (find-file org-todo-file)))

(global-set-key "\C-cn" '(lambda ()
                           (interactive)
                           (find-file org-notes-file)))


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
(setq indent-tabs-mode nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(global-set-key (kbd "C-c r") (lambda ()
                                (interactive)
                                (revert-buffer t t t)
                                (message "buffer is reverted")))

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq create-lockfiles nil)

(setq-default left-margin-width 0 right-margin-width 0)
(set-window-buffer nil (current-buffer))

(setq show-paren-mode t)

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
(load "~/.emacs.d/lisp/acme-theme.el")

(global-set-key (kbd "M-z") 'zap-up-to-char)

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
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package cider
  :ensure t)

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode))

(defun remap-faces-default-attributes ()
  (let ((family (face-attribute 'default :family))
        (height (face-attribute 'default :height)))
    (mapcar (lambda (face)
              (face-remap-add-relative
               face :family family :weight 'normal :height height))
            (face-list))))

(add-hook 'minibuffer-setup-hook 'remap-faces-default-attributes)
(add-hook 'change-major-mode-after-body-hook 'remap-faces-default-attributes)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
	 (quote
		("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(org-agenda-files
	 (quote
		("~/Dropbox/org/inbox.org" "~/Dropbox/org/maybe.org" "~/Dropbox/org/notes.org" "~/Dropbox/org/todo.org")))
 '(package-selected-packages
	 (quote
		(forth-mode magit ag zzz-to-char zap-to-char zop-to-char json-mode gradle-mode ob-clojurescript which-key cider soft-stone-theme stekene minimal-theme monochrome monochrome-theme farmhouse-theme basic-theme eziam-common eziam-theme github-modern-theme dockerfile-mode markdown-mode go-mode eink-theme github-theme yaml-mode use-package terraform-mode solarized-theme projectile git-commit editorconfig auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
