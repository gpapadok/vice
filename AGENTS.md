# AGENTS.md

This file provides guidance to coding agents when working with code in this repository.

## Project overview

`vice` (VIm-like Commands Extension for Emacs) is a small single-file Emacs
Lisp package that adds Vim-style text-editing commands (e.g. `da(`, `di(`,
`dd`, `yy`, `p`, `J`) to Emacs without replacing Emacs's native keybindings.
It's distributed as a minor mode, `vice-mode`.

## Commands

Run the ERT test suite:

```sh
make test
```

This runs `emacs -Q --batch -L . -l vice-mode-test.el -f ert-run-tests-batch-and-exit`,
loading the package fresh (no user init file) and executing all `ert-deftest`
forms in `vice-mode-test.el`.

To run a single test, invoke Emacs batch mode directly with `-f`:

```sh
emacs -Q --batch -L . -l vice-mode-test.el -f ert-run-tests-batch-and-exit \
  --eval "(ert-run-tests-batch-and-exit 'vice-kill-surrounding-sexp-test)"
```

or more simply, load `vice-mode-test.el` in an interactive Emacs session and
use `M-x ert RET <test-name> RET`.

There is no separate lint/build step; `Makefile` only defines `test`.

## Architecture

- `vice-mode.el` — the entire package. All commands operate on the sexp
  (balanced expression) surrounding point, or on the current line, using
  Emacs's built-in `forward-sexp`/`backward-up-list`/kill-ring primitives.
  Structure within the file:
  - **Helpers**: `vice--key` (builds a keybinding under the configurable
    `vice-key-prefix`, default `"C-c v"`), `vice--defvar-keymap` (macro that
    builds a keymap from an alist of key-suffix/command pairs),
    `vice--save-point` (macro to run a body and restore point afterward),
    `vice--backward-up-list` (safe wrapper that won't error at top level or
    when point sits on a leading paren), and `vice--surrounding-sexp-bounds`
    (the core primitive — returns `(start end)` of the sexp enclosing point;
    nearly every interactive command is built on top of this).
  - **Commands**: interactive `vice-*` functions (kill/yank/comment
    surrounding or inside sexp, insert line above/below, join lines,
    replace sexp from kill-ring, line-level kill/yank), each marked
    `;;;###autoload`.
  - **Minor mode**: `vice-map` is built via `vice--defvar-keymap` from an
    alist mapping key suffixes (e.g. `"w"`, `"C-w"`, `"M-w"`) to the
    commands above; `vice-mode` is a global minor mode defined with
    `define-minor-mode` using that keymap.
- `vice-mode-test.el` — ERT tests. Defines two macros to keep tests terse:
  `test-with` (apply a function at a buffer position on a "before" string,
  assert the buffer matches an "after" string) and `multiple-tests-with`
  (run several `test-with` cases against the same function). Indentation
  rules for these macros (and `vice--defvar-keymap`) are declared in
  `.dir-locals.el` for correct `indent-for-tab-command` behavior in Emacs.
- `README.org` documents every public command and its default keybinding
  (all under the `C-c v` prefix) — update it when adding or rebinding a
  command.
