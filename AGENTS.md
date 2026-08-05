# AGENTS.md

This file provides guidance to coding agents when working with code in this repository.

## Project overview

`vice` (VIm-like Commands Extension for Emacs) is a small single-file Emacs
Lisp package that brings Vim's composable `operator + text object` editing
(e.g. `da(`, `di(`, `ci"`) to Emacs without modal state and without replacing
Emacs's native keybindings. It's distributed as a minor mode, `vice-mode`.

A single prefix key (`vice-key-prefix`, default `C-c v`) runs `vice-dispatch`,
which reads a whole operation of the form `[count] operator [a|i] object`.

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
  --eval "(ert-run-tests-batch-and-exit 'vice-dispatch-test)"
```

or more simply, load `vice-mode-test.el` in an interactive Emacs session and
use `M-x ert RET <test-name> RET`.

There is no separate lint/build step; `Makefile` only defines `test`.

## Architecture

- `vice-mode.el` — the entire package, organized as layered data plus the
  dispatch that reads them. From the bottom up:
  - **Text objects**: `vice--pair-bounds` (bracket pair enclosing point, via
    `syntax-ppss` and the syntax table, any bracket type, `a`/`i`, count),
    `vice--string-bounds` (string via `syntax-ppss`), `vice--thing-bounds`
    (wraps `bounds-of-thing-at-point`), and `vice--treesit-bounds`
    (tree-sitter node, `fboundp`-guarded so it is a no-op without a parser).
    `vice-object-alist` maps an object character to an ordered list of
    provider specs — `(:pair OPEN)`, `(:string QUOTE)`, `(:thing THING)`,
    `(:treesit THING)` — and `vice--object-bounds` tries them in order,
    returning the first `(start end)` found.
  - **Operators**: `vice--op-*` functions of `(start end)`, collected in
    `vice-operator-alist` (keys `d y ; v r =`), applied by `vice--apply`
    inside an `atomic-change-group` (one undo step per operation).
  - **Dispatch**: `vice-dispatch` (the sole `;;;###autoload` command) reads
    `[count] operator [a|i] object` with `read-char-exclusive`, resolves
    bounds, and applies the operator.
  - **Minor mode**: `vice-map` binds `vice-key-prefix` to `vice-dispatch`;
    `vice-mode` is a global minor mode using `vice-map`.
- `vice-mode-test.el` — ERT tests that call the internal bounds/dispatch
  functions directly and feed key sequences to `vice-dispatch` via
  `unread-command-events`. The tree-sitter test is guarded with `skip-unless`
  so the suite passes without a grammar installed.
- `README.org` documents the grammar (operators, objects, examples) — update
  it when adding or rebinding an operator or object.
