.PHONY: test
test:
	emacs --batch -L . -l unity-launcher-api-tests -f ert-run-tests-batch
