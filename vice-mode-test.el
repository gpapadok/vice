;;; vice-mode-test.el --- Tests for vice commands -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Giorgos Papadokostakis

;; Author: Giorgos Papadokostakis <giorgos.papadokostakis@proton.me>

;; This file is not part of GNU Emacs.

;; This file is free software.

;;; Code:

(require 'vice-mode)

(defun vice-test--pair-bounds (before position &rest args)
  "Return the result of `vice--pair-bounds' called with ARGS.
Run in a temp buffer holding BEFORE with point at POSITION, in
`emacs-lisp-mode' so that all bracket types carry paren syntax."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert before)
    (goto-char position)
    (apply #'vice--pair-bounds args)))

(ert-deftest vice--pair-bounds-test ()
  ;; "(foo (bar a b c))"  inner pair spans 6..16, outer 1..17
  (let ((s "(foo (bar a b c))"))
    ;; nearest enclosing pair, around vs inside
    (should (equal (vice-test--pair-bounds s 8 'a) '(6 17)))
    (should (equal (vice-test--pair-bounds s 8 'i) '(7 16)))
    ;; on the opening and closing parens resolve to the same pair
    (should (equal (vice-test--pair-bounds s 6 'a) '(6 17)))
    (should (equal (vice-test--pair-bounds s 16 'a) '(6 17)))
    ;; count ascends to the outer pair
    (should (equal (vice-test--pair-bounds s 8 'a nil 2) '(1 18)))))

(ert-deftest vice--pair-bounds-type-test ()
  ;; "(foo [bar] baz)"  bracket pair 6..10, paren pair 1..15
  (let ((s "(foo [bar] baz)"))
    ;; nearest pair from inside the brackets is the brackets
    (should (equal (vice-test--pair-bounds s 8 'a) '(6 11)))
    ;; asking for a paren specifically skips the brackets (Vim da( )
    (should (equal (vice-test--pair-bounds s 8 'a ?\() '(1 16)))
    (should (equal (vice-test--pair-bounds s 8 'a ?\[) '(6 11)))))

(ert-deftest vice--pair-bounds-outside-test ()
  ;; No enclosing pair, and end of buffer: nil, never an error.
  (should (null (vice-test--pair-bounds "  foo bar" 4 'a)))
  (should (null (vice-test--pair-bounds "(foo bar)" 10 'a))))

(defun vice-test--object-bounds (before position object modifier &optional count)
  "Return `vice--object-bounds' for OBJECT MODIFIER COUNT.
Run in an `emacs-lisp-mode' temp buffer holding BEFORE with point at
POSITION, so brackets carry paren syntax and \"\\\"\" carries string
syntax."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert before)
    (goto-char position)
    (vice--object-bounds object modifier count)))

(ert-deftest vice--object-bounds-pair-test ()
  ;; Delimiter objects route through vice--pair-bounds; ) resolves the
  ;; same pair as (, and m takes the nearest pair of any type.
  (let ((s "(foo [bar])"))
    (should (equal (vice-test--object-bounds s 8 ?\( 'a) '(1 12)))
    (should (equal (vice-test--object-bounds s 8 ?\) 'a) '(1 12)))
    (should (equal (vice-test--object-bounds s 8 ?m 'a) '(6 11)))
    (should (equal (vice-test--object-bounds s 8 ?\[ 'i) '(7 10)))))

(ert-deftest vice--object-bounds-string-test ()
  ;; "(f \"foo bar\")": string spans 4..12, contents 5..11
  (let ((s "(f \"foo bar\")"))
    (should (equal (vice-test--object-bounds s 6 ?\" 'a) '(4 13)))
    (should (equal (vice-test--object-bounds s 6 ?\" 'i) '(5 12)))
    ;; on the opening quote resolves the same string
    (should (equal (vice-test--object-bounds s 4 ?\" 'a) '(4 13)))))

(ert-deftest vice--object-bounds-thing-test ()
  ;; word: i is the word, a extends over trailing whitespace
  (let ((s "foo   bar"))
    (should (equal (vice-test--object-bounds s 2 ?w 'i) '(1 4)))
    (should (equal (vice-test--object-bounds s 2 ?w 'a) '(1 7))))
  ;; symbol spans the hyphenated name in emacs-lisp-mode
  (should (equal (vice-test--object-bounds "foo-bar baz" 3 ?s 'i) '(1 8))))

(ert-deftest vice--object-bounds-unknown-test ()
  ;; An unregistered object character yields nil, not an error.
  (should (null (vice-test--object-bounds "(foo)" 3 ?z 'a))))

(defun vice-test--run (before position operator object modifier)
  "Apply OPERATOR to OBJECT/MODIFIER at POSITION in BEFORE.
Return (BUFFER-STRING POINT).  Run in an `emacs-lisp-mode' temp buffer."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert before)
    (goto-char position)
    (pcase (vice--object-bounds object modifier nil)
      (`(,start ,end)
       (vice--apply (cdr (assq operator vice-operator-alist)) start end)))
    (list (buffer-string) (point))))

(ert-deftest vice--apply-kill-test ()
  ;; d a ( deletes the pair; d i ( deletes its contents
  (should (equal (car (vice-test--run "(foo (bar))" 8 ?d ?\( 'a)) "(foo )"))
  (should (equal (car (vice-test--run "(foo (bar))" 8 ?d ?\( 'i)) "(foo ())")))

(ert-deftest vice--apply-save-test ()
  ;; y copies without modifying the buffer
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(foo (bar))")
    (goto-char 8)
    (let (kill-ring kill-ring-yank-pointer)
      (pcase (vice--object-bounds ?\( 'a nil)
        (`(,start ,end)
         (vice--apply #'vice--op-save start end)))
      (should (equal (buffer-string) "(foo (bar))"))
      (should (equal (current-kill 0) "(bar)")))))

(ert-deftest vice--apply-replace-test ()
  ;; r swaps the object for the kill-ring head
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(foo (bar))")
    (goto-char 8)
    (let ((kill-ring '("BAZ")) kill-ring-yank-pointer)
      (pcase (vice--object-bounds ?\( 'a nil)
        (`(,start ,end)
         (vice--apply #'vice--op-replace start end)))
      (should (equal (buffer-string) "(foo BAZ)")))))

(ert-deftest vice--apply-select-test ()
  ;; v activates the region spanning the object
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(foo (bar))")
    (goto-char 8)
    (pcase (vice--object-bounds ?\( 'a nil)
      (`(,start ,end)
       (vice--apply #'vice--op-select start end)))
    ;; region-active-p depends on transient-mark-mode (off in batch),
    ;; so assert on the underlying point and mark directly.
    (should mark-active)
    (should (= (point) 6))
    (should (= (mark t) 11))))

(defun vice-test--dispatch (before position keys)
  "Run `vice-dispatch' reading KEYS at POSITION in BEFORE.
KEYS is a key-sequence string fed through `unread-command-events'.
Return (BUFFER-STRING POINT).  Run in an `emacs-lisp-mode' temp buffer."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert before)
    (goto-char position)
    (let ((unread-command-events (listify-key-sequence keys)))
      (vice-dispatch))
    (list (buffer-string) (point))))

(ert-deftest vice-dispatch-test ()
  ;; operator + a/i + object, end to end through the reader
  (should (equal (car (vice-test--dispatch "(foo (bar))" 8 "da(")) "(foo )"))
  (should (equal (car (vice-test--dispatch "(foo (bar))" 8 "di(")) "(foo ())"))
  ;; a leading digit sets the count and reaches the outer pair
  (should (equal (car (vice-test--dispatch "(foo (bar))" 8 "2da(")) ""))
  ;; a word object
  (should (equal (car (vice-test--dispatch "foo bar" 2 "diw")) " bar")))

(ert-deftest vice-dispatch-save-test ()
  ;; y copies without modifying the buffer
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(foo (bar))")
    (goto-char 8)
    (let (kill-ring kill-ring-yank-pointer
          (unread-command-events (listify-key-sequence "ya(")))
      (vice-dispatch)
      (should (equal (buffer-string) "(foo (bar))"))
      (should (equal (current-kill 0) "(bar)")))))

(ert-deftest vice-dispatch-no-object-test ()
  ;; No object at point: buffer unchanged, no error.
  (should (equal (car (vice-test--dispatch "  foo" 4 "da(")) "  foo")))

(ert-deftest vice-map-binds-dispatch-test ()
  ;; The prefix key runs the grammar reader.
  (should (eq (lookup-key vice-map (kbd vice-key-prefix)) #'vice-dispatch)))

(ert-deftest vice--object-bounds-treesit-function-test ()
  ;; The f object resolves a whole function (a) or its body (i) via
  ;; tree-sitter.  Skipped when the python grammar is not installed.
  (skip-unless (and (fboundp 'treesit-language-available-p)
                    (treesit-language-available-p 'python)
                    (fboundp 'python-ts-mode)))
  (with-temp-buffer
    (python-ts-mode)
    (insert "def foo(a, b):\n    return a + b\n")
    (goto-char 20)                       ; inside the body
    (should (equal (vice--object-bounds ?f 'a) '(1 32)))
    (should (equal (vice--object-bounds ?f 'i) '(20 32)))))

(ert-deftest vice--object-bounds-treesit-fallback-test ()
  ;; With no tree-sitter parser, f falls back to the defun thing.
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun foo () 1)")
    (goto-char 8)
    (should (equal (vice--object-bounds ?f 'a) '(1 17)))))

;;; vice-mode-test.el ends here
