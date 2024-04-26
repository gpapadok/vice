;;; vice-mode.el --- Vi like commands extension for emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Giorgos Papadokostakis

;; Author: Giorgos Papadokostakis <giorgos.papadokostakis@proton.me>
;; Created: 13 December 2023
;; Name; Vice-mode
;; Version: 0.1.0
;; Keywords: vi, commands

;; This file is not part of GNU Emacs.

;; This file is free software.

;;; Code:
;; helpers
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

(defmacro vice--save-point (&rest body)
  "Returns to the starting position after the execution of `body`."
  `(let ((p (point))
	 (result (progn ,@body)))
     (goto-char p)
     result))

(defun vice--backward-up-list ()
  (when (not (char-equal (char-after (point)) ?\())
    (condition-case nil
	(backward-up-list)
      (error nil))))

(defun vice--surrounding-sexp-bounds ()
  (vice--save-point
   (vice--backward-up-list)
   (let ((start (point)))
     (forward-sexp 1)
     (list start (point)))))

;; commands

(defun vice-kill-surrounding-sexp () ; da(
  "Delete the sexp surrounding point."
  (interactive)
  (pcase (vice--surrounding-sexp-bounds)
    (`(,start ,end)
     (kill-region start end))))

(defun vice-kill-inside-sexp ()	; di(
  "Delete inside the sexp surrounding point."
  (interactive)
  (pcase (vice--surrounding-sexp-bounds)
    (`(,start ,end)
     (kill-region (1+ start) (1- end)))))

(defun vice-yank-surrounding-sexp () ; ya(
  "Yank the sexp surrounding point."
  (interactive)
  (pcase (vice--surrounding-sexp-bounds)
    (`(,start ,end)
     (kill-ring-save start end))))

(defun vice-yank-inside-sexp ()	; yi(
  "Yank the content of sexp surrounding point."
  (interactive)
  (pcase (vice--surrounding-sexp-bounds)
    (`(,start ,end)
     (kill-ring-save (1+ start) (1- end)))))

(defun vice-comment-surrounding-sexp ()
  "Comment the sexp surrounding point."
  (interactive)
  (pcase (vice--surrounding-sexp-bounds)
    (`(,start ,end)
     (comment-region start end))))

(defun vice-insert-line-below () ; o
  "Same as hitting enter at end of line."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(defun vice-insert-line () ; O
  "Insert an indented line at the same line as point."
  (interactive)
  (back-to-indentation)
  (newline-and-indent)
  (forward-line -1)
  (indent-for-tab-command))

(defun vice-join-line-one-space () ; J
  (interactive)
  (move-end-of-line 1)
  (kill-line)
  (just-one-space))

(defun vice-join-line-no-space () ; gJ
  (interactive)
  (move-end-of-line 1)
  (kill-line)
  (delete-horizontal-space))

(defun vice-replace-sexp ()
  (interactive)
  (vice--backward-up-list)
  (yank)
  (kill-sexp))

(defun vice-save-line () ; yy
  "Copies current line."
  (interactive)
  (vice--save-point
    (move-beginning-of-line 1)
    (let ((region-start (point)))
      (forward-line)
      (kill-ring-save region-start (point)))))

(defun vice-yank-line () ; p
  "Pastes a line."
  (interactive)
  (vice--save-point
    (move-beginning-of-line 1)
    (yank)))

(defun vice-save-end-of-line ()
  "Copies from current point to the end of line."
  (interactive)
  (vice--save-point
   (let ((opoint (point)))
     (move-end-of-line 1)
     (kill-ring-save opoint (point)))))

(defun vice-kill-line-at-point () ; dd
  "Deletes line of current point."
  (interactive)
  (vice--save-point
   (move-beginning-of-line 1)
   (let ((opoint (point)))
     (forward-line)
     (kill-region opoint (point)))))

;;

(defvar-keymap vice-map
  '(("C-x w" . vice-kill-surrounding-sexp)
    ("C-c M-k" . vice-kill-inside-sexp)
    ("C-x M-w" . vice-yank-surrounding-sexp)
    ("C-c M-w" . vice-yank-inside-sexp)
    ("C-x M-;" . vice-comment-surrounding-sexp)
    ("C-x C-j" . vice-insert-line-below)
    ("C-x M-j" . vice-insert-line)
    ("C-x C-k" . vice-join-line-one-space)
    ("C-c C-k" . vice-join-line-no-space)
    ("C-x C-y" . vice-replace-sexp)
    ("C-x M-l" . vice-save-line)
    ("C-x C-M-l" . vice-yank-line)
    ("C-c C-e" . vice-save-end-of-line)
    ("C-x C-M-k" . vice-kill-line-at-point)))

(define-minor-mode vice-mode
  "Minor mode with vi like commands."
  :global t
  :lighter " vi"
  :keymap vice-map)

(provide 'vice-mode)
;;; vi-binds-mode.el ends here
