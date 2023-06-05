  $ llc ../../../../../src/array.ll -o output.s
  $ gcc -no-pie stdlib.o output.s || echo $?
  $ ./a.out 