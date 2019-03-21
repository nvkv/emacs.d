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
