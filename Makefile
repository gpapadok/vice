.PHONY: test

test:
	emacs -Q --batch -L . -l vice-mode-test.el -f ert-run-tests-batch-and-exit
