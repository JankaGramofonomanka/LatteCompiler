all:
	stack build --copy-bins

	cp -f src/latc latc
	chmod +x latc
	cp -f src/latc_llvm latc_llvm
	chmod +x latc_llvm

rebuild:
	rm -rf ./.stack-work
	make all

clean:
	rm ./latc
	rm ./latc_llvm

	rm ./bin/latc_exe
	rm ./bin/latc_llvm_exe

