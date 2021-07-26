# LatteCompiler

This is a compiler of the Latte programming language, whose description can be 
found here: https://www.mimuw.edu.pl/~ben/Zajecia/Mrj2020/Latte/
(in Polish)

- To build the project, use the Makefile, ie. run
  ```
  make
  ```

- To verify semantic correctness of a program, run
  ```
  ./latc [FILE]
  ```
  where `FILE` is the file with code you want to verify.
  If the code is correct, the output will be
  ```
  OK
  ```
  If the code is incorrect, the output will be
  ```
  ERROR
  [MSG]
  ```
  where `MSG` is a message specifying what is incorrect about the code.



- (NOT IMPLEMENTED YET)
  You can compile your code to llvm by running
  ```
  ./latc_llvm [FILE]
  ```
  where `FILE` is the fil with code you want to compile.
  the llvm code will be outputted.

