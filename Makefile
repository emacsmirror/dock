.PHONY: test run

test:
	emacs --batch -L . -l dock-tests -f ert-run-tests-batch

run:
	emacs -L . -l dock --no-desktop --debug-init
