.PHONY: test compile

test:
	emacs -Q --batch -L . -l vice-mode-test.el -f ert-run-tests-batch-and-exit

compile:
	emacs -Q --batch -L . --eval "(setq byte-compile-error-on-warn t)" -f batch-byte-compile vice-mode.el
	rm -f vice-mode.elc
