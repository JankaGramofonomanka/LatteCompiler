all:
	stack build --copy-bins

rebuild:
	rm -rf ./.stack-work
	stack build --copy-bins

clean:
	rm ./latc
	rm ./latc_llvm

	#./scripts/cleanup_tests
