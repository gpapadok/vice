;;; vice-mode-test.el --- Tests for vice commands -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Giorgos Papadokostakis

;; Author: Giorgos Papadokostakis <giorgos.papadokostakis@proton.me>

;; This file is not part of GNU Emacs.

;; This file is free software.

;;; Code:

(require 'vice-mode)

(defmacro test-with (fn at position before -> after)
  "Helper macro to easily test functions that operate on text.
Simple syntax to apply operation FN AT POSITION on BEFORE
text and test equality of result with AFTER.
Example use:
\(test-with #'upcase-word at 0 \"hello\" -> \"HELLO\"\)"
  (if (or (not (eql at 'at)) (not (eql -> '->)))
      (error "Malformed test-with form")
    `(with-temp-buffer
       (insert ,before)
       (goto-char ,position)
       (funcall ,fn)
       (should (string= (buffer-string) ,after)))))

(defmacro multiple-tests-with (fn &rest tests)
  "Helper macro to test a function on multiple before after text pairs.
Basically extends `test-with` to test an operator FN on multiple
before after pairs \(TESTS\)"
  `(progn
     ,@(mapcar (lambda (test)
                 `(test-with ,fn
                    ,@test))
               tests)))

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

(ert-deftest vice-kill-surrounding-sexp-test ()
  (multiple-tests-with #'vice-kill-surrounding-sexp
    (at 8 "(foo (bar a b c))" -> "(foo )")  ; inside sexp
    (at 6 "(foo (bar a b c))" -> "(foo )")  ; on opening paren
    (at 16 "(foo (bar a b c))" -> "(foo )") ; on closing paren
    (at 2 "(foo (bar a b c))" -> "")        ; inside higher level sexp
    (at 2 "     (foo (bar a b c))"-> "     (foo (bar a b c))"))) ; outside sexp

(ert-deftest vice-kill-inside-sexp-test ()
  (multiple-tests-with #'vice-kill-inside-sexp
    (at 8 "(foo (bar a b c))" -> "(foo ())")  ; inside inner sexp
    (at 6 "(foo (bar a b c))" -> "(foo ())")  ; on opening paren
    (at 16 "(foo (bar a b c))" -> "(foo ())") ; on closing paren
    (at 2 "(foo (bar a b c))" -> "()")  ; inside higher level sexp
    (at 2 "     (foo (bar a b c)))" -> "     (foo (bar a b c)))"))) ; outside sexp

(ert-deftest vice-save-surrounding-sexp-test ()
  (test-with (lambda ()
               (vice-save-surrounding-sexp)
               (end-of-buffer)
               (yank))
    at 8 "(foo (bar a b c))" -> "(foo (bar a b c))(bar a b c)"))

(ert-deftest vice-save-inside-sexp-test ()
  (test-with (lambda ()
               (vice-save-inside-sexp)
               (beginning-of-buffer)
               (yank))
    at 2 "(foo (bar a b c))" -> "foo (bar a b c)(foo (bar a b c))"))

(ert-deftest vice-save-inside-sexp-outside-test ()
  "Regression: saving inside a sexp outside any list must not error."
  (with-temp-buffer
    (insert "     (foo (bar a b c))")
    (goto-char 2)
    (let (kill-ring kill-ring-yank-pointer)
      (vice-save-inside-sexp)
      (should (null kill-ring))
      (should (string= (buffer-string) "     (foo (bar a b c))")))))

(ert-deftest vice-replace-sexp-outside-test ()
  "Regression: replacing outside any list must not alter the buffer."
  (with-temp-buffer
    (insert "  (foo bar)")
    (goto-char 1)
    (let ((kill-ring '("XXX"))
          kill-ring-yank-pointer)
      (vice-replace-sexp)
      (should (string= (buffer-string) "  (foo bar)")))))

(ert-deftest vice-kill-line-at-point-test ()
  (test-with #'vice-kill-line-at-point
    at 17
    "(defun inc (n)
  \"A doc string\"
  (+ n 1))"
    ->
    "(defun inc (n)
  (+ n 1))"))

;;; vice-mode-test.el ends here
