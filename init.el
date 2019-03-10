(require 'package)

(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "~/go/bin/")
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))

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

(show-paren-mode 1)

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
    (set-frame-font "Fira Code Retina-18")
		;; specify font for all unicode characters
		(when (member "Symbola" (font-family-list))
			(set-fontset-font t 'unicode "Symbola" nil 'prepend))))


(load "~/.emacs.d/lisp/russian-nowinkeys")
(setq default-input-method "russian-no-windows")
(load "~/.emacs.d/lisp/the-color-theme.el")

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

(use-package swift-mode
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
		(rainbow-delimeters groovy-mode markdown-mode+ web-mode tide swift-mode forth-mode magit ag zzz-to-char zap-to-char zop-to-char json-mode gradle-mode ob-clojurescript which-key cider soft-stone-theme stekene minimal-theme monochrome monochrome-theme farmhouse-theme basic-theme eziam-common eziam-theme github-modern-theme dockerfile-mode markdown-mode go-mode eink-theme github-theme yaml-mode use-package terraform-mode solarized-theme projectile git-commit editorconfig auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Do not kill, delete
(defun please-delete-word (arg)
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word arg)
     (point))))

(defun please-backward-delete-word (arg)
  (interactive "p")
  (please-delete-word (- arg)))

(defun please-delete-line (arg)
  (interactive "p")
  (delete-region (point) (progn (end-of-visual-line arg) (point))))


(defun del/delete-whole-line (&optional arg)
  (interactive "p")
  (or arg (setq arg 1))
  (if (and (> arg 0) (eobp) (save-excursion (forward-visible-line 0) (eobp)))
      (signal 'end-of-buffer nil))
  (if (and (< arg 0) (bobp) (save-excursion (end-of-visible-line) (bobp)))
      (signal 'beginning-of-buffer nil))
  (unless (eq last-command 'delete-region)
    (kill-new "")
    (setq last-command 'delete-region))
  (cond ((zerop arg)
	 ;; We need to kill in two steps, because the previous command
	 ;; could have been a kill command, in which case the text
	 ;; before point needs to be prepended to the current kill
	 ;; ring entry and the text after point appended.  Also, we
	 ;; need to use save-excursion to avoid copying the same text
	 ;; twice to the kill ring in read-only buffers.
	 (save-excursion
	   (delete-region (point) (progn (forward-visible-line 0) (point))))
	 (delete-region (point) (progn (end-of-visible-line) (point))))
	((< arg 0)
	 (save-excursion
	   (delete-region (point) (progn (end-of-visible-line) (point))))
	 (delete-region (point)
		      (progn (forward-visible-line (1+ arg))
			     (unless (bobp) (backward-char))
			     (point))))
	(t
	 (save-excursion
	   (delete-region (point) (progn (forward-visible-line 0) (point))))
	 (delete-region (point)
		      (progn (forward-visible-line arg) (point))))))


(defun del/delete-line (arg)
	(interactive "p")
  (delete-region (point)
								 (progn
									 (if arg
											 (forward-visible-line (prefix-numeric-value arg))
										 (if (eobp)
												 (signal 'end-of-buffer nil))
										 (let ((end
														(save-excursion
															(end-of-visible-line) (point))))
											 (if (or (save-excursion
																 ;; If trailing whitespace is visible,
																 ;; don't treat it as nothing.
																 (unless show-trailing-whitespace
																	 (skip-chars-forward " \t" end))
																 (= (point) end))
															 (and del/delete-whole-line (bolp)))
													 (forward-visible-line 1)
												 (goto-char end))))
									 (point))))

(defun please-delete-line-backward ()
  (interactive)
  (let (p1 p2)
    (setq p1 (point))
    (beginning-of-line 1)
    (setq p2 (point))
    (delete-region p1 p2)))

(global-set-key (kbd "C-S-k") 'please-delete-line-backward)
(global-set-key (kbd "C-k") 'del/delete-line)
(global-set-key (kbd "M-d") 'please-delete-word)
(global-set-key (kbd "<M-backspace>") 'please-backward-delete-word)
