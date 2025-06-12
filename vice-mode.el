;;; vice-mode.el --- VIm Like Commands Extension for emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Giorgos Papadokostakis

;; Author: Giorgos Papadokostakis <giorgos.papadokostakis@proton.me>
;; Created: 13 December 2023
;; Name; Vice
;; Version: 0.1.0
;; Keywords: vi, commands

;; This file is not part of GNU Emacs.

;; This file is free software.

;;; Code:

;; Custom

(defgroup vice nil
  "Manipulate text with Vim-like commands."
  :group 'convenience
  :prefix "vice-")

(defcustom vice-key-prefix "C-c v"
  "Key prefix for vice commands.")

;; Helpers

(defun vice--key (key)
  (kbd (concat vice-key-prefix " " key)))

(defmacro vice--defvar-keymap (name keybinds &optional docstring)
  "Define a keymap from an alist of sequences and functions."
  (let ((map (gensym))
        (bind (gensym)))
    `(progn
       (defvar ,name nil ,docstring)
       (setq ,name
             (let ((,map (make-keymap)))
               (dolist (,bind ,keybinds)
                 (define-key ,map (vice--key (car ,bind)) (cdr ,bind)))
               ,map)))))

(defmacro vice--save-point (&rest body)
  "Return to the starting position after the execution of BODY."
  `(let ((p (point))
         (result (progn ,@body)))
     (goto-char p)
     result))

(defun vice--backward-up-list ()
  "Like `backward-up-list` but safer.
Doesn't work on top of a leading paren and doesn't error on top level form."
  (when (not (char-equal (char-after (point)) ?\())
    (condition-case nil
        (backward-up-list)
      (error nil))))

(defun vice--surrounding-sexp-bounds ()
  "Return the start and end point of the surrounding sexp."
  (vice--save-point
   (let ((p (point)))
     (vice--backward-up-list)
     (let ((start (point)))
       (unless (and (= p start)
                    (not (char-equal (char-after start) ?\())) ; Only if inside a sexp
         (forward-sexp 1))
       (list start (point))))))

;; Commands

;;;###autoload
(defun vice-kill-surrounding-sexp () ; da(
  "Delete the sexp surrounding point."
  (interactive)
  (pcase (vice--surrounding-sexp-bounds)
    (`(,start ,end)
     (kill-region start end))))

;;;###autoload
(defun vice-kill-inside-sexp () ; di(
  "Delete inside the sexp surrounding point."
  (interactive)
  (pcase (vice--surrounding-sexp-bounds)
    (`(,start ,end)
     (if (< start end)
         (kill-region (1+ start) (1- end))))))

;;;###autoload
(defun vice-yank-surrounding-sexp () ; ya(
  "Yank the sexp surrounding point."
  (interactive)
  (pcase (vice--surrounding-sexp-bounds)
    (`(,start ,end)
     (kill-ring-save start end))))

;;;###autoload
(defun vice-yank-inside-sexp () ; yi(
  "Yank the content of sexp surrounding point."
  (interactive)
  (pcase (vice--surrounding-sexp-bounds)
    (`(,start ,end)
     (kill-ring-save (1+ start) (1- end)))))

;;;###autoload
(defun vice-comment-surrounding-sexp ()
  "Comment the sexp surrounding point."
  (interactive)
  (pcase (vice--surrounding-sexp-bounds)
    (`(,start ,end)
     (comment-region start end))))

;;;###autoload
(defun vice-insert-line-below () ; o
  "Same as hitting enter at end of line."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

;;;###autoload
(defun vice-insert-line () ; O
  "Insert an indented line at the same line as point."
  (interactive)
  (back-to-indentation)
  (newline-and-indent)
  (forward-line -1)
  (indent-for-tab-command))

;;;###autoload
(defun vice-join-line-one-space ()
  "Joins current line with next leaving only one space between.
Like vi J."
  (interactive)
  (move-end-of-line 1)
  (kill-line)
  (just-one-space))

;;;###autoload
(defun vice-join-line-no-space ()
  "Joins current line with next leaving no whitespace.
Like vi gJ."
  (interactive)
  (move-end-of-line 1)
  (kill-line)
  (delete-horizontal-space))

;;;###autoload
(defun vice-replace-sexp ()
  "Replace sorrounding sexp by yanking from kill buffer."
  (interactive)
  (vice--backward-up-list)
  (yank)
  (kill-sexp))

;;;###autoload
(defun vice-save-line ()
  "Copies current line.
Like vi yy."
  (interactive)
  (vice--save-point
   (move-beginning-of-line 1)
   (let ((region-start (point)))
     (forward-line)
     (kill-ring-save region-start (point)))))

;;;###autoload
(defun vice-yank-line ()
  "Pastes a line.
Like vi p."
  (interactive)
  (vice--save-point
   (move-beginning-of-line 1)
   (yank)))

;;;###autoload
(defun vice-save-end-of-line ()
  "Copies from current point to the end of line."
  (interactive)
  (vice--save-point
   (let ((opoint (point)))
     (move-end-of-line 1)
     (kill-ring-save opoint (point)))))

;;;###autoload
(defun vice-kill-line-at-point ()
  "Deletes line of current point.
Like Vi dd."
  (interactive)
  (vice--save-point
   (move-beginning-of-line 1)
   (let ((opoint (point)))
     (forward-line)
     (kill-region opoint (point)))))

;; Minor mode

(vice--defvar-keymap vice-map
  '(("w" . vice-kill-surrounding-sexp)
    ("C-w" . vice-kill-inside-sexp)
    ("M-w" . vice-yank-surrounding-sexp)
    ("C-w" . vice-yank-inside-sexp)
    (";" . vice-comment-surrounding-sexp)
    ("j" . vice-insert-line-below)
    ("M-j" . vice-insert-line)
    ("k" . vice-join-line-one-space)
    ("M-k" . vice-join-line-no-space)
    ("y" . vice-replace-sexp)
    ("l" . vice-kill-line-at-point)
    ("M-l" . vice-save-line)
    ("M-l" . vice-yank-line)
    ("e" . vice-save-end-of-line)))

;;;###autoload
(define-minor-mode vice-mode
  "Minor mode with Vi like commands."
  :global t
  :lighter " vice"
  :keymap vice-map)

(provide 'vice-mode)
;;; vice-mode.el ends here
