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

(ert-deftest vice-yank-surrounding-sexp-test ()
  (test-with (lambda ()
               (vice-yank-surrounding-sexp)
               (end-of-buffer)
               (yank))
    at 8 "(foo (bar a b c))" -> "(foo (bar a b c))(bar a b c)"))

(ert-deftest vice-yank-inside-sexp-test ()
  (test-with (lambda ()
               (vice-yank-inside-sexp)
               (beginning-of-buffer)
               (yank))
    at 2 "(foo (bar a b c))" -> "foo (bar a b c)(foo (bar a b c))"))

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
