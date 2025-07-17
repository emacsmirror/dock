.PHONY: test
test:
	emacs --batch -L . -l dock-tests -f ert-run-tests-batch
