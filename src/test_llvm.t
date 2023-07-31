  $ llc ../../../../../src/array.ll -o output.s
  $ clang -no-pie stdlib.o output.s "../../../../../src/std.ll" 
  $ ./a.out < ../../../../../src/test_read.input