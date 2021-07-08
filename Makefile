all:
	stack build --copy-bins

clean:
	rm ./latc
	rm ./latc_llvm

	./cleanup_tests
