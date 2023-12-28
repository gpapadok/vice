;;; vim-binds.el --- Vim like commands for emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Giorgos Papadokostakis

;; Author: Giorgos Papadokostakis <giorgos.papadokostakis@proton.me>
;; Created: 13 December 2023
;; Name; Vim-binds
;; Version: 0.1.0
;; Keywords: vim, commands

;; This file is not part of GNU Emacs.

;; This file is free software.

;;; Code:
(defmacro save-point (&rest body)
  "Returns to the starting position after the execution of `body`."
  `(let ((p (point))
	 (result (progn ,@body)))
     (goto-char p)
     result))

(defun backward-up-list-safe ()
  (when (not (char-equal (char-after (point)) ?\())
    (condition-case nil
	(backward-up-list)
      (error nil))))

(defun surrounding-sexp-bounds ()
  (save-point
   (backward-up-list-safe)
   (let ((start (point)))
     (forward-sexp 1)
     (list start (point)))))

(defun kill-surrounding-sexp () ; da(
  "Delete the sexp surrounding point."
  (interactive)
  (pcase (surrounding-sexp-bounds)
    (`(,start ,end)
     (kill-region start end))))

(defun kill-inside-sexp ()	; di(
  "Delete inside the sexp surrounding point."
  (interactive)
  (pcase (surrounding-sexp-bounds)
    (`(,start ,end)
     (kill-region (1+ start) (1- end)))))

(defun yank-surrounding-sexp () ; ya(
  "Yank the sexp surrounding point."
  (interactive)
  (pcase (surrounding-sexp-bounds)
    (`(,start ,end)
     (kill-ring-save start end))))

(defun yank-inside-sexp ()	; yi(
  "Yank the content of sexp surrounding point."
  (interactive)
  (pcase (surrounding-sexp-bounds)
    (`(,start ,end)
     (kill-ring-save (1+ start) (1- end)))))

(defun comment-surrounding-sexp ()
  "Comment the sexp surrounding point."
  (interactive)
  (pcase (surrounding-sexp-bounds)
    (`(,start ,end)
     (comment-region start end))))

(defun insert-line-below () ; o
  "Same as hitting enter at end of line."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(defun insert-line () ; O
  "Insert an indented line at the same line as point."
  (interactive)
  (back-to-indentation)
  (newline-and-indent)
  (forward-line -1)
  (indent-for-tab-command))

(defun join-line-one-space () ; J
  (interactive)
  (move-end-of-line 1)
  (kill-line)
  (just-one-space))

(defun join-line-no-space () ; gJ
  (interactive)
  (move-end-of-line 1)
  (kill-line)
  (delete-horizontal-space))

(defun replace-sexp ()
  (interactive)
  (backward-up-list-safe)
  (yank)
  (kill-sexp))

(defun save-line ()
  "Copies current line."
  (interactive)
  (save-point
    (move-beginning-of-line 1)
    (let ((region-start (point)))
      (forward-line)
      (kill-ring-save region-start (point)))))

(defun yank-line ()
  "Pastes a line."
  (interactive)
  (save-point
    (move-beginning-of-line 1)
    (yank)))

(defun save-end-of-line ()
  "Copies from current point to the end of line."
  (interactive)
  (save-point
   (let ((opoint (point)))
     (move-end-of-line 1)
     (kill-ring-save opoint (point)))))

(defun kill-line-at-point ()
  "Deletes line of current point."
  (interactive)
  (save-point
   (move-beginning-of-line 1)
   (let ((opoint (point)))
     (forward-line)
     (kill-region opoint (point)))))

(defmacro defvar-keymap (name keybinds &optional docstring)
  "Define a keymap dynamic var from an alist of key sequences
and functions."
  (let ((map (gensym))
	(bind (gensym)))
    `(defvar ,name
       (let ((,map (make-keymap)))
	 (dolist (,bind ,keybinds)
	   (define-key ,map (kbd (car ,bind)) (cdr ,bind)))
	 ,map)
       ,docstring)))

(defvar-keymap vim-binds-map
  '(("C-x M-k" . kill-surrounding-sexp)
    ("C-c M-k" . kill-inside-sexp)
    ("C-x M-w" . yank-surrounding-sexp)
    ("C-c M-w" . yank-inside-sexp)
    ("C-x M-;" . comment-surrounding-sexp)
    ("C-x C-j" . insert-line-below)
    ("C-x M-j" . insert-line)
    ("C-x C-k" . join-line-one-space)
    ("C-c C-k" . join-line-no-space)
    ("C-x C-y" . replace-sexp)
    ("C-x M-l" . save-line)
    ("C-x C-M-l" . yank-line)
    ("C-c C-e" . save-end-of-line)
    ("C-x C-M-k" . kill-line-at-point)))

(define-minor-mode vim-binds-mode
  "Minor mode with vim like commands."
  :global t
  :lighter " vi"
  :keymap vim-binds-map)

(provide 'vim-binds-mode)

;;; vim-binds.el ends here
