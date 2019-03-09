;; acme-theme.el -- A theme inspired by the classic colours of the ACME editor.
;; Copyright (C) 2014 , Tslil Clingman

;; Version: 0.1
;; Package-Requires: ((emacs "24"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of Emacs.

(deftheme the-color-theme
  "A theme inspired by the classic colours of the ACME editor but significantly diverged since")

(let ((bg "#F7F7F7")
      (lazy "#D1D1C0")
      (fg "black")
      (yellow "#EAEA9E")
      (red "#AA0000")
      (blue "#EAFFFF")
      (purple "#fee9e9")
      (green "#508050")
      (mud "#99994C"))
  (custom-theme-set-faces
   'the-color-theme
   `(default ((t (:background ,bg :foreground ,fg ))))
   '(cursor ((t (:background "black"))))
   '(escape-glyph ((t (:foreground "black"))))
   '(minibuffer-prompt ((t (:foreground "black" :weight bold))))
   '(highlight ((t (:foreground "grey30"))))
   `(region ((t (:background ,yellow :foreground "black"))))
   `(secondary-selection ((t (:background ,red))))
   '(trailing-whitespace ((t (:background "grey"))))
   '(font-lock-builtin-face ((t (:foreground "black" :slant normal))))
   '(font-lock-comment-delimiter-face ((default (:inherit (font-lock-comment-face)))))
   '(font-lock-comment-face ((t (:foreground "dark red"))))
   '(font-lock-constant-face ((t (:foreground "black"))))
   `(font-lock-doc-face ((t (:foreground "dark green"))))
   '(font-lock-function-name-face ((t (:foreground "dark blue"))))
   '(font-lock-keyword-face ((t (:foreground "black"))))
   '(font-lock-negation-char-face ((t (:foreground "black"))))
   '(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
   '(font-lock-string-face ((t (:foreground "dark green"))))
   '(font-lock-type-face ((t (:foreground "black"))))
   '(font-lock-variable-name-face ((t (:foreground "black"))))
   `(sh-quoted-exec ((t (:foreground "black"))))
	 `(sh-heredoc ((t (:foreground "dark green"))))
   '(font-lock-warning-face ((t (:foreground "black"))))
   '(button ((t (:inherit (link)))))
   '(link ((t (:underline (:color foreground-color :style line) :foreground "black"))))
   '(link-visited ((default (:inherit (link))) (((class color) (background light)) (:foreground "magenta4")) (((class color) (background dark)) (:foreground "violet"))))
   `(fringe ((t (:background ,bg :foreground "black"))))
   '(header-line ((default (:inherit (mode-line))) (((type tty)) (:underline (:color foreground-color :style line) :inverse-video nil)) (((class color grayscale) (background light)) (:box nil :foreground "grey20" :background "grey90")) (((class color grayscale) (background dark)) (:box nil :foreground "grey90" :background "grey20")) (((class mono) (background light)) (:underline (:color foreground-color :style line) :box nil :inverse-video nil :foreground "black" :background "white")) (((class mono) (background dark)) (:underline (:color foreground-color :style line) :box nil :inverse-video nil :foreground "white" :background "black"))))
   '(tooltip ((((class color)) (:inherit (variable-pitch) :foreground "black" :background "lightyellow")) (t (:inherit (variable-pitch)))))
   `(mode-line ((t (:background ,blue :foreground "black" :box 2))))
   '(mode-line-buffer-id ((t (:foreground "black" :weight bold))))
   '(mode-line-emphasis ((t (:foreground "black"))))
   '(mode-line-highlight ((t (:weight bold :box nil :foreground "black"))))
   `(mode-line-inactive ((t (:background ,blue :foreground "#393939" :weight normal :box t))))
   `(isearch ((t (:background ,yellow))))
   `(isearch-fail ((t (:background ,red))))
   `(lazy-highlight ((t (:foreground "black" :weight bold :background ,blue))))
   `(match ((t (:background ,yellow))))
   '(next-error ((t (:inherit (region)))))
   '(query-replace ((t (:inherit (isearch)))))

   ;;'(shadow ((((class color grayscale) (min-colors 88) (background light)) (:foreground "grey50")) (((class color grayscale) (min-colors 88) (background dark)) (:foreground "grey70")) (((class color) (min-colors 8) (background light)) (:foreground "green")) (((class color) (min-colors 8) (background dark)) (:foreground "yellow"))))
   ;;'(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
   ;;'(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))]

   `(show-paren-match ((t (:weight bold :background ,purple))))
   `(show-paren-mismatch ((t (:background ,red))))
   `(sp-show-pair-match-face ((t (:weight bold :background ,purple))))
   `(sp-show-pair-mismatch-face ((t (:background ,red))))

   `(helm-header ((t (:foreground "black" :background ,bg :weight bold))))
   `(helm-source-header ((t (:foreground "black" :background ,blue :box t))))
   `(helm-selection ((t (:inverse-video t))))
   `(helm-visible-mark ((t (:foreground ,fg :background ,green :weight bold))))
   `(helm-match ((t (:foreground "black" :background ,bg :weight bold))))
   `(helm-M-x-key ((t (:foreground "black" :background ,purple))))
   ;; '(helm-separator ((t (:foreground "black" :background ,bg))))
   ;; '(helm-time-zone-current ((t (:foreground "black" :background ,bg))))
   ;; '(helm-time-zone-home ((t (:foreground "black" :background ,bg))))
   `(helm-buffer-not-saved ((t (:foreground "black" :background ,red))))
   `(helm-buffer-process ((t (:foreground "black" :background ,blue))))
   `(helm-buffer-saved-out ((t (:foreground "black" :background ,purple))))
   `(helm-buffer-size ((t (:foreground "grey40" :background ,bg))))
   `(helm-ff-dotted-directory ((t (:foreground "black" :background ,bg :weight bold))))
   `(helm-ff-directory ((t (:foreground "black" :background ,bg :weight bold))))
   `(helm-ff-file ((t (:foreground "black" :background ,bg))))
   `(helm-ff-executable ((t (:foreground ,green :background ,bg))))
   `(helm-ff-invalid-symlink ((t (:foreground ,red :background ,bg))))
   `(helm-ff-symlink ((t (:foreground ,yellow :background ,bg :weight bold))))
   `(helm-ff-prefix ((t (:foreground "black" :background ,purple))))
   ;; '(helm-grep-cmd-line ((t (:foreground ,fg :background ,bg))))
   ;; '(helm-grep-file ((t (:foreground ,fg :background ,bg))))
   `(helm-grep-finish ((t (:foreground "black" :background ,bg))))
   ;; '(helm-grep-lineno ((t (:foreground ,fg :background ,bg))))
   ;; '(helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))
   ;; '(helm-grep-running ((t (:foreground "black" :background ,bg))))
   ;; '(helm-moccur-buffer ((t (:foreground "black" :background ,bg))))
   ;; '(helm-source-go-package-godoc-description ((t (:foreground "black"))))
   ;; '(helm-bookmark-w3m ((t (:foreground "black"))))
   `(helm-locate-finish ((t (:foreground "black" :background ,yellow))))
   `(helm-swoop-target-line-block-face ((t (:foreground "black" :background ,yellow))))
   `(helm-swoop-target-line-face ((t (:foreground "black" :background ,yellow))))
   '(helm-swoop-target-word-face ((t (:foreground "black" :weight bold :slant italic))))

   `(company-echo-common ((t (:foreground "black" :background "grey" :weight bold))))
   `(company-preview ((t (:background "grey" :foreground "black"))))
   `(company-preview-common ((t (:foreground "black" :foreground "grey" :weight bold))))
   `(company-preview-search ((t (:foreground "black" :background ,yellow))))
   `(company-scrollbar-bg ((t (:background ,mud))))
   `(company-scrollbar-fg ((t (:background ,lazy))))
   `(company-tooltip ((t (:foreground "black" :background "grey"))))
   `(company-tooltip-search ((t (:foreground "black" :background ,yellow))))
   `(company-tooltip-annotation ((t (:foreground "black" :background "grey" :slant italic))))
   `(company-tooltip-common ((t (:foreground "black" :background "grey" :weight bold))))
   `(company-tooltip-common-selection ((t (:foreground "black" :background ,blue :weight bold))))
   ;;`(company-tooltip-mouse ((t (:inherit highlight))))
   `(company-tooltip-selection ((t (:background ,blue :foreground "black"))))
   ;;`(company-template-field ((t (:inherit region))))

   '(gnus-header-content ((t (:foreground "black"))))
   '(gnus-header-from ((t (:foreground "black"))))
   '(gnus-header-name ((t (:foreground "black"))))
   '(gnus-header-subject ((t (:foreground "black" :weight bold))))

   '(gnus-summary-low-ancient ((t (:foreground "black"))))
   '(gnus-summary-normal-ancient ((t (:foreground "black"))))
   '(gnus-summary-high-ancient ((t (:foreground "black"))))

   '(gnus-summary-low-read ((t (:foreground "black"))))
   '(gnus-summary-normal-read ((t (:foreground "black"))))
   '(gnus-summary-high-read ((t (:foreground "black"))))

   '(gnus-summary-low-unread ((t (:foreground "black" :underline t))))
   '(gnus-summary-normal-unread ((t (:foreground "black" :underline t))))
   '(gnus-summary-high-unread ((t (:foreground "black" :underline t))))

   '(gnus-summary-low-undownloaded ((t (:foreground "black"))))
   '(gnus-summary-normal-undownloaded ((t (:foreground "black"))))
   '(gnus-summary-high-undownloaded ((t (:foreground "black"))))

   '(gnus-summary-low-ticked ((t (:foreground "black" :background "grey"))))
   '(gnus-summary-normal-ticked ((t (:foreground "black" :background "grey"))))
   '(gnus-summary-high-ticked ((t (:foreground "black" :background "grey"))))

   `(gnus-summary-cancelled ((t (:foreground "black" :background ,bg :slant italic))))

   '(gnus-summary-selected-face ((t (:foreground "black" :weight bold))))

   `(gnus-group-mail-1 ((t (:foreground "black" :background ,bg))))
   `(gnus-group-mail-2 ((t (:foreground "black" :background ,bg))))
   `(gnus-group-mail-3 ((t (:foreground "black" :background ,bg))))
   `(gnus-group-mail-4 ((t (:foreground "black" :background ,bg))))
   `(gnus-group-mail-5 ((t (:foreground "black" :background ,bg))))
   `(gnus-group-mail-6 ((t (:foreground "black" :background ,bg))))
   `(gnus-group-mail-low ((t (:foreground "black" :background ,bg))))
   `(gnus-group-mail-1-empty ((t (:foreground "black" :background ,bg))))
   `(gnus-group-mail-2-empty ((t (:foreground "black" :background ,bg))))
   `(gnus-group-mail-3-empty ((t (:foreground "black" :background ,bg))))
   `(gnus-group-mail-4-empty ((t (:foreground "black" :background ,bg))))
   `(gnus-group-mail-5-empty ((t (:foreground "black" :background ,bg))))
   `(gnus-group-mail-6-empty ((t (:foreground "black" :background ,bg))))
   `(gnus-group-mail-low-empty ((t (:foreground "black" :background ,bg))))

	 '(clojure-keyword-face ((t (:foreground "grey29"))))

   '(font-latex-bold-face ((t (:weight bold))))
   '(font-latex-italic-face ((t (:slant italic))))
   `(font-latex-math-face ((t (:foreground "black"
                                           :box (:line-width 2 :color ,lazy
                                                             :style pressed-button)
                                           :foreground "black"))))
   '(font-latex-string-face ((t (:foreground "black"))))
   '(font-latex-verbatim-face ((t (:foreground "black"))))
   '(font-latex-sedate-face ((t (:foreground "black"))))
   '(font-latex-warning-face ((t (:foreground "black"))))
   '(font-latex-sectioning-5-face ((t (:foreground "black" :weight normal))))))

(provide-theme 'the-color-theme)
