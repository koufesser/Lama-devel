  $ llc ../../../../../src/array.ll -o output.s
  $ clang -no-pie stdlib.o output.s || echo $?
  $ ./a.out < ../../../../../src/test_read.input