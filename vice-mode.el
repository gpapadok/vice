;;; vice-mode.el --- VIm Like Commands Extension for emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Giorgos Papadokostakis

;; Author: Giorgos Papadokostakis <giorgos.papadokostakis@proton.me>
;; Created: 13 December 2023
;; Name: Vice
;; Version: 0.1.0
;; Keywords: vi, commands

;; This file is not part of GNU Emacs.

;; This file is free software.

;;; Commentary:

;; Vice is an attempt to close the gap between Emacs vanilla keybinds
;; and the fluidity some of Vi commands give.  Primarily, it is
;; supposed to make it easier to manipulate structured expressions.
;; All operations that vice provides are possible with Emacs vanilla
;; commands but may require multiple operations and can often feel
;; clunky.  Vice is meant to make a lot of common text editing
;; operations feel streamlined and smooth.

;;; Code:

(require 'thingatpt)
(require 'treesit nil t)

;; Custom

(defgroup vice nil
  "Manipulate text with Vim-like commands."
  :group 'convenience
  :prefix "vice-")

(defcustom vice-key-prefix "C-c v"
  "Key prefix for vice commands.")

;; Helpers

(defmacro vice--save-point (&rest body)
  "Evaluate BODY and return to the starting position afterward.
The position is tracked with a marker, so it is restored correctly even
when BODY inserts or deletes text before point."
  (let ((marker (gensym "marker"))
        (result (gensym "result")))
    `(let ((,marker (point-marker))
           (,result (progn ,@body)))
       (goto-char ,marker)
       (set-marker ,marker nil)
       ,result)))

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
(:treesit THING); `vice--object-bounds' tries them in order and returns
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

(defun vice--op-change (start end)
  "Kill the region START..END and leave point in the resulting hole."
  (kill-region start end)
  (goto-char start))

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
    (?c . vice--op-change)
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
The operator is one of `vice-operator-alist' (d y c ; v r =), the
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

;; Commands

(defun vice--operate-on-object (operator object modifier)
  "Apply OPERATOR to OBJECT/MODIFIER at point when its bounds exist.
OPERATOR is a function as stored in `vice-operator-alist'."
  (pcase (vice--object-bounds object modifier)
    (`(,start ,end)
     (vice--apply operator start end))))

;;;###autoload
(defun vice-kill-surrounding-sexp () ; da(
  "Delete the sexp surrounding point."
  (interactive)
  (vice--operate-on-object #'vice--op-kill ?m 'a))

;;;###autoload
(defun vice-kill-inside-sexp () ; di(
  "Delete inside the sexp surrounding point."
  (interactive)
  (vice--operate-on-object #'vice--op-kill ?m 'i))

;;;###autoload
(defun vice-save-surrounding-sexp () ; ya(
  "Saves the sexp surrounding point to the kill ring."
  (interactive)
  (vice--operate-on-object #'vice--op-save ?m 'a))

;;;###autoload
(defun vice-save-inside-sexp () ; yi(
  "Saves the content of the sexp surrounding point to the kill ring."
  (interactive)
  (vice--operate-on-object #'vice--op-save ?m 'i))

;;;###autoload
(defun vice-comment-surrounding-sexp ()
  "Comment the sexp surrounding point."
  (interactive)
  (vice--operate-on-object #'vice--op-comment ?m 'a))

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
  "Replace surrounding sexp by yanking from the kill ring."
  (interactive)
  (vice--operate-on-object #'vice--op-replace ?m 'a))

;;;###autoload
(defun vice-save-line ()
  "Saves the current line to the kill ring.
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
(defun vice-kill-end-of-line ()
  "Deletes from current point to the end of line."
  (interactive)
  (vice--save-point
   (let ((opoint (point)))
     (move-end-of-line 1)
     (kill-region opoint (point)))))

;;;###autoload
(defun vice-save-end-of-line ()
  "Saves from point to the end of line to the kill ring."
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

(defvar vice-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd vice-key-prefix) #'vice-dispatch)
    map)
  "Keymap for `vice-mode'.
The `vice-key-prefix' key runs `vice-dispatch', which reads a full
operation of the form [count] operator [a|i] object.")

(defcustom vice-legacy-key-prefix "C-c V"
  "Key prefix for the classic single-key vice commands.
Used by `vice-install-legacy-bindings'.  It is a sibling of, not nested
under, `vice-key-prefix', which is bound to `vice-dispatch'."
  :type 'string
  :group 'vice)

(defconst vice--legacy-bindings
  '(("w" . vice-kill-surrounding-sexp)
    ("C-w" . vice-kill-inside-sexp)
    ("M-w" . vice-save-surrounding-sexp)
    ("M-W" . vice-save-inside-sexp)
    (";" . vice-comment-surrounding-sexp)
    ("j" . vice-insert-line-below)
    ("M-j" . vice-insert-line)
    ("k" . vice-join-line-one-space)
    ("M-k" . vice-join-line-no-space)
    ("y" . vice-replace-sexp)
    ("l" . vice-kill-line-at-point)
    ("M-l" . vice-save-line)
    ("C-l" . vice-yank-line)
    ("e" . vice-kill-end-of-line)
    ("M-e" . vice-save-end-of-line))
  "Alist of key suffixes to the classic vice commands.")

;;;###autoload
(defun vice-install-legacy-bindings (&optional prefix)
  "Bind the classic vice commands under PREFIX in `vice-map'.
PREFIX defaults to `vice-legacy-key-prefix'.  These are the pre-grammar
single-key commands; `vice-dispatch' on `vice-key-prefix' supersedes
them, so they are opt-in."
  (interactive)
  (let ((prefix (or prefix vice-legacy-key-prefix)))
    (dolist (binding vice--legacy-bindings)
      (define-key vice-map
                  (kbd (concat prefix " " (car binding)))
                  (cdr binding)))))

;;;###autoload
(define-minor-mode vice-mode
  "Minor mode with Vi like commands."
  :global t
  :lighter " vice"
  :keymap vice-map)

(provide 'vice-mode)
;;; vice-mode.el ends here
