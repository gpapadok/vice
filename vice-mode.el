;;; vice-mode.el --- VIm-like Commands Extension -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Giorgos Papadokostakis

;; Author: Giorgos Papadokostakis <giorgos.papadokostakis@proton.me>
;; Assisted-by: Claude Sonnet 5 <noreply@anthropic.com>
;; Created: 13 December 2023
;; Version: 0.2.0
;; Keywords: convenience, emulations
;; URL: https://github.com/gpapadok/vice
;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Vice brings Vim's composable "operator + text object" editing to
;; Emacs without modal state.  The `vice-key-prefix' key (default
;; "C-c v") runs `vice-dispatch', which reads a whole operation of the
;; form
;;
;;     [count] operator [a|i] object
;;
;; For example "C-c v d a (" deletes the surrounding parentheses,
;; "C-c v y i \"" copies the contents of a string, and
;; "C-c v 2 d a (" deletes two levels of enclosing parentheses.
;;
;; Operators come from `vice-operator-alist' (d y ; v r =) and
;; objects from `vice-object-alist' (brackets, strings, word, symbol,
;; paragraph, function, ...).  Objects resolve through syntax-table,
;; thing-at-point, and tree-sitter providers, so the grammar works
;; beyond Lisp.

;;; Code:

(require 'thingatpt)
(require 'treesit nil t)

;; Custom

(defgroup vice nil
  "Manipulate text with Vim-like commands."
  :group 'convenience
  :prefix "vice-")

(defcustom vice-key-prefix "C-c v"
  "Key prefix bound to `vice-dispatch' in `vice-map'."
  :type 'string
  :group 'vice
  :set (lambda (sym val)
         (let ((old (and (boundp sym) (symbol-value sym))))
           (set-default sym val)
           (when (boundp 'vice-map)
             (when old (define-key vice-map (kbd old) nil))
             (define-key vice-map (kbd val) #'vice-dispatch)))))

;; Text objects

(defun vice--open-delimiter-p (pos)
  "Return non-nil when the character at POS opens a balanced expression.
Recognizes any open-paren-class delimiter (\"(\", \"[\", \"{\") per the
current syntax table, and is nil at the end of the buffer."
  (let ((c (char-after pos)))
    (and c (eq (char-syntax c) ?\())))

(defun vice--pair-open-positions ()
  "Return positions of enclosing opening delimiters, innermost first.
When point sits on an opening delimiter it is treated as the innermost
enclosing pair."
  (let ((opens (reverse (nth 9 (syntax-ppss)))))
    (if (vice--open-delimiter-p (point))
        (cons (point) opens)
      opens)))

(defun vice--pair-bounds (modifier &optional open count)
  "Return (START END) of the bracket pair enclosing point, or nil.
MODIFIER is `a' to include the delimiters or `i' for their contents.
OPEN, when non-nil, is the opening-delimiter character to seek; otherwise
the nearest enclosing pair of any type is used.  COUNT ascends COUNT
levels (default 1)."
  (let* ((positions (vice--pair-open-positions))
         (positions (if open
                        (seq-filter (lambda (p) (eq (char-after p) open))
                                    positions)
                      positions))
         (start (nth (1- (or count 1)) positions)))
    (when start
      (save-excursion
        (goto-char start)
        (let ((end (progn (forward-sexp 1) (point))))
          (pcase modifier
            ('a (list start end))
            ('i (if (< (1+ start) (1- end))
                    (list (1+ start) (1- end))
                  (list (1+ start) (1+ start))))))))))

(defun vice--string-bounds (modifier &optional quote)
  "Return (START END) of the string enclosing point, or nil.
MODIFIER is `a' to include the quote characters or `i' for the
contents.  QUOTE, when non-nil, requires that opening quote character."
  (let* ((ppss (syntax-ppss))
         (start (cond
                 ((nth 3 ppss) (nth 8 ppss))              ; inside a string
                 ((eq (char-syntax (or (char-after) ?\s)) ?\")
                  (point)))))                             ; on an opening quote
    (when (and start (or (null quote) (eq (char-after start) quote)))
      (save-excursion
        (goto-char start)
        (let ((end (progn (forward-sexp 1) (point))))
          (pcase modifier
            ('a (list start end))
            ('i (list (1+ start) (1- end)))))))))

(defun vice--thing-bounds (modifier thing)
  "Return (START END) for THING at point, or nil.
THING is a symbol understood by `bounds-of-thing-at-point'.  For
MODIFIER `a', a word or symbol extends over trailing whitespace, as
in Vim's `aw'."
  (let ((bounds (bounds-of-thing-at-point thing)))
    (when bounds
      (let ((start (car bounds))
            (end (cdr bounds)))
        (when (and (eq modifier 'a) (memq thing '(word symbol)))
          (save-excursion
            (goto-char end)
            (skip-chars-forward " \t")
            (setq end (point))))
        (list start end)))))

(defun vice--treesit-thing-spec (thing)
  "Return a spec `treesit-thing-at-point' accepts for THING, or nil.
For `defun', fall back to the major mode's `treesit-defun-type-regexp'
when tree-sitter thing settings do not define it."
  (pcase thing
    ('defun (cond
             ((and (fboundp 'treesit-thing-defined-p)
                   (treesit-thing-defined-p 'defun (treesit-language-at (point))))
              'defun)
             ((bound-and-true-p treesit-defun-type-regexp))))
    (_ (and (fboundp 'treesit-thing-defined-p)
            (treesit-thing-defined-p thing (treesit-language-at (point)))
            thing))))

(defun vice--treesit-bounds (modifier thing)
  "Return (START END) for the tree-sitter THING enclosing point, or nil.
Returns nil (so callers fall back to another provider) when tree-sitter
is unavailable, the buffer has no parser, or THING is not resolvable.
For MODIFIER `i' the bounds shrink to the node's \"body\" field when it
has one."
  (when (and (fboundp 'treesit-parser-list)
             (fboundp 'treesit-thing-at-point)
             (treesit-parser-list))
    (let* ((spec (vice--treesit-thing-spec thing))
           (node (and spec (ignore-errors (treesit-thing-at-point spec 'nested)))))
      (when node
        (pcase modifier
          ('a (list (treesit-node-start node) (treesit-node-end node)))
          ('i (let ((inner (or (treesit-node-child-by-field-name node "body")
                               node)))
                (list (treesit-node-start inner) (treesit-node-end inner)))))))))

(defvar vice-object-alist
  '((?\( (:pair ?\())
    (?\) (:pair ?\())
    (?\[ (:pair ?\[))
    (?\] (:pair ?\[))
    (?\{ (:pair ?\{))
    (?\} (:pair ?\{))
    (?m  (:pair nil))
    (?\" (:string ?\"))
    (?'  (:string ?'))
    (?w  (:thing word))
    (?s  (:thing symbol))
    (?p  (:thing paragraph))
    (?f  (:treesit defun) (:thing defun)))
  "Alist mapping an object character to a list of provider specs.
Each spec is one of (:pair OPEN), (:string QUOTE), (:thing THING), or
\(:treesit THING); `vice--object-bounds' tries them in order and returns
the first bounds found.  Both members of a delimiter pair map to the
same object.")

(defun vice--object-bounds (object modifier &optional count)
  "Return (START END) for OBJECT relative to point, or nil.
OBJECT is a character key in `vice-object-alist'.  MODIFIER is `a' or
`i'.  COUNT applies to providers that support it."
  (seq-some (lambda (spec)
              (pcase spec
                (`(:pair ,open) (vice--pair-bounds modifier open count))
                (`(:string ,quote) (vice--string-bounds modifier quote))
                (`(:thing ,thing) (vice--thing-bounds modifier thing))
                (`(:treesit ,thing) (vice--treesit-bounds modifier thing))))
            (cdr (assq object vice-object-alist))))

;; Operators

(defun vice--op-kill (start end)
  "Kill the region START..END."
  (kill-region start end))

(defun vice--op-save (start end)
  "Copy the region START..END to the kill ring."
  (kill-ring-save start end))

(defun vice--op-comment (start end)
  "Comment or uncomment the region START..END."
  (comment-or-uncomment-region start end))

(defun vice--op-select (start end)
  "Activate the region START..END."
  (goto-char start)
  (push-mark end nil t))

(defun vice--op-replace (start end)
  "Replace the region START..END with the head of the kill ring."
  (goto-char start)
  (delete-region start end)
  (yank))

(defun vice--op-indent (start end)
  "Indent the region START..END."
  (indent-region start end))

(defvar vice-operator-alist
  '((?d . vice--op-kill)
    (?y . vice--op-save)
    (?\; . vice--op-comment)
    (?v . vice--op-select)
    (?r . vice--op-replace)
    (?= . vice--op-indent))
  "Alist mapping an operator character to a function of two arguments.
The function receives the START and END of the region to act on.")

(defun vice--apply (operator start end)
  "Run OPERATOR on the region START..END as a single undo step.
OPERATOR is a function of two arguments as stored in
`vice-operator-alist'."
  (atomic-change-group
    (funcall operator start end)))

;; Dispatch

(defun vice--operator-keys ()
  "Return the operator characters as a display string."
  (mapconcat (lambda (e) (char-to-string (car e))) vice-operator-alist ""))

;;;###autoload
(defun vice-dispatch (&optional arg)
  "Read and run a vice operation: [count] operator [a|i] object.
The operator is one of `vice-operator-alist' (d y ; v r =), the
modifier is `a' (around) or `i' (inside), and the object is a key in
`vice-object-alist'.  A numeric prefix ARG, or leading digits, sets the
count where the object supports it.  \\[keyboard-quit] aborts at any
point."
  (interactive "P")
  (let ((count (and arg (prefix-numeric-value arg)))
        (char (read-char-exclusive (format "vice [%s]:" (vice--operator-keys)))))
    (unless count
      (let ((n 0) (seen nil))
        (while (<= ?0 char ?9)
          (setq n (+ (* n 10) (- char ?0))
                seen t
                char (read-char-exclusive (format "vice %d:" n))))
        (when seen (setq count n))))
    (let ((operator (assq char vice-operator-alist)))
      (cond
       ((null operator)
        (message "vice: %s is not an operator" (single-key-description char)))
       (t
        (let* ((op-str (char-to-string char))
               (mchar (read-char-exclusive (format "vice %s [a/i]:" op-str)))
               (modifier (pcase mchar (?a 'a) (?i 'i))))
          (if (null modifier)
              (message "vice: expected `a' or `i'")
            (let ((object (read-char-exclusive (format "vice %s%c:" op-str mchar))))
              (pcase (vice--object-bounds object modifier count)
                (`(,start ,end)
                 (vice--apply (cdr operator) start end))
                (_ (message "vice: no %s object at point"
                            (single-key-description object))))))))))))

;; Minor mode

(defvar vice-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd vice-key-prefix) #'vice-dispatch)
    map)
  "Keymap for `vice-mode'.
The `vice-key-prefix' key runs `vice-dispatch', which reads a full
operation of the form [count] operator [a|i] object.")

;;;###autoload
(define-minor-mode vice-mode
  "Minor mode with Vi like commands."
  :global t
  :lighter " vice"
  :keymap vice-map)

(provide 'vice-mode)
;;; vice-mode.el ends here
